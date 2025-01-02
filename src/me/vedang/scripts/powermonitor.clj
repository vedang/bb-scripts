(ns me.vedang.scripts.powermonitor
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def thermal-paths
  ["/sys/class/thermal/thermal_zone0/temp"
   "/sys/class/thermal/thermal_zone1/temp"])

(defn get-timestamp
  []
  (.format (LocalDateTime/now) DateTimeFormatter/ISO_LOCAL_DATE_TIME))

(defn read-temp-file
  [path]
  (try (when (fs/exists? path)
         (-> (slurp path)
             str/trim
             (Double/parseDouble)
             (/ 1000.0))) ; Convert to Celsius
       (catch Exception e
         (println "Error reading temperature from" path ":" (.getMessage e))
         nil)))

(defn get-cpu-temp
  []
  (->> thermal-paths
       (keep read-temp-file)
       (apply max 0.0)))

(defn get-memory-info
  []
  (let [meminfo (slurp "/proc/meminfo")
        total (-> (re-find #"MemTotal:\s+(\d+)" meminfo)
                  second
                  Long/parseLong)
        free (-> (re-find #"MemFree:\s+(\d+)" meminfo)
                 second
                 Long/parseLong)
        buffers (-> (re-find #"Buffers:\s+(\d+)" meminfo)
                    second
                    Long/parseLong)
        cached (-> (re-find #"Cached:\s+(\d+)" meminfo)
                   second
                   Long/parseLong)]
    (double (* 100 (/ (- total (+ free buffers cached)) total)))))

(defn get-cpu-percent
  []
  (let [cpu-info1 (-> (slurp "/proc/stat")
                      (str/split-lines)
                      first
                      (str/split #"\s+"))
        _ (Thread/sleep 1000) ; Wait 1 second
        cpu-info2 (-> (slurp "/proc/stat")
                      (str/split-lines)
                      first
                      (str/split #"\s+"))
        parse-values (fn [info]
                       (->> (subvec info 1)
                            (take 7)
                            (mapv #(Long/parseLong %))))
        values1 (parse-values cpu-info1)
        values2 (parse-values cpu-info2)
        idle1 (nth values1 3)
        idle2 (nth values2 3)
        total1 (apply + values1)
        total2 (apply + values2)
        idle-diff (- idle2 idle1)
        total-diff (- total2 total1)]
    (* 100.0 (/ (- total-diff idle-diff) total-diff))))

(defn collect-metrics
  []
  {:timestamp (get-timestamp),
   :cpu-percent (get-cpu-percent),
   :memory-percent (get-memory-info),
   :temperature (get-cpu-temp)})

(defn write-csv-header
  [file]
  (when-not (fs/exists? file)
    (spit file "timestamp,cpu_percent,memory_percent,temperature\n")))

(defn log-metrics
  [file metrics]
  (spit file
        (format "%s,%.2f,%.2f,%.2f\n"
                (:timestamp metrics)
                (:cpu-percent metrics)
                (:memory-percent metrics)
                (:temperature metrics))
        :append
        true))

(defn print-metrics
  [metrics]
  (printf "\rCPU: %.1f%% | Memory: %.1f%% | Temp: %.1f°C"
          (:cpu-percent metrics)
          (:memory-percent metrics)
          (:temperature metrics))
  (flush))

(defn monitor
  [{:keys [duration interval output]}]
  (let [interval (or interval 1)
        output (or output "power_metrics.csv")]
    (write-csv-header output)
    (println "Starting monitoring, saving to" output)
    (println "Press Ctrl+C to stop monitoring")
    (try (let [start-time (System/currentTimeMillis)]
           (loop []
             (when (or (nil? duration)
                       (< (- (System/currentTimeMillis) start-time)
                          (* duration 1000)))
               (let [metrics (collect-metrics)]
                 (log-metrics output metrics)
                 (print-metrics metrics)
                 (Thread/sleep (* interval 1000))
                 (recur)))))
         (catch Exception e (println "\nError:" (.getMessage e)))
         (finally (println "\nMonitoring stopped")
                  (println "Metrics saved to" output)))))

(def cli-options
  {:duration {:coerce :long, :desc "Duration to monitor in seconds"},
   :interval
   {:coerce :double, :default 1.0, :desc "Sampling interval in seconds"},
   :output {:default "power_metrics.csv", :desc "Output file path"}})

(defn -main [& args] (monitor (cli/parse-opts args {:spec cli-options})))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

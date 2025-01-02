(ns me.vedang.scripts.powermonitor
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :refer [sh]]
            [clojure.string :as str])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def thermal-paths
  ["/sys/class/thermal/thermal_zone0/temp"
   "/sys/class/thermal/thermal_zone1/temp"
   "/sys/class/thermal/thermal_zone2/temp"
   "/sys/class/thermal/thermal_zone3/temp"
   "/sys/class/thermal/thermal_zone4/temp"
   "/sys/class/thermal/thermal_zone5/temp"
   "/sys/class/thermal/thermal_zone6/temp"])

(defn get-timestamp
  []
  (.format (LocalDateTime/now) DateTimeFormatter/ISO_LOCAL_DATE_TIME))

(defn read-temp-file
  [path]
  (try (when (fs/exists? path)
         (-> (:out (sh "cat" path))
             str/trim
             parse-double
             (/ 1000.0))) ; Convert to Celsius
       (catch Exception e
         (println "Error reading temperature from" path ":" (.getMessage e))
         nil)))

(defn get-cpu-temp
  []
  (->> thermal-paths
       (keep read-temp-file)
       (apply max 0.0)))

(defn get-mem-info
  "See: https://www.baeldung.com/linux/proc-meminfo for more information"
  [meminfo] ;; [tag: mem_usage]
  (let [mem-find (fn [metric]
                   (let [re-pat (re-pattern (str metric ":\\s+(\\d+)"))]
                     (-> (re-find re-pat meminfo)
                         second
                         parse-long)))
        total (mem-find "MemTotal")
        free (mem-find "MemFree")
        buffers (mem-find "Buffers")
        cached (mem-find "Cached")]
    (double (* 100 (/ (- total (+ free buffers cached)) total)))))

(defn get-memory-usage
  [] ;; [ref: mem_usage]
  (get-mem-info (:out (sh "cat" "/proc/meminfo"))))

(defn- get-cpu-info
  [proc-stat] ;; [ref: cpu_usage]
  (zipmap [:user :nice :system :idle :iowait :irq :softirq]
          (mapv parse-long
            (take 7
                  (-> proc-stat
                      (str/split-lines)
                      first
                      (str/split #"\s+")
                      rest)))))

(defn get-cpu-percent
  "See https://www.linuxhowtos.org/System/procstat.htm for more detailed information"
  ;; [tag: cpu_usage]
  []
  (let [first-read (get-cpu-info (:out (sh "cat" "/proc/stat")))
        _ (Thread/sleep 1000) ; Wait 1 second
        second-read (get-cpu-info (:out (sh "cat" "/proc/stat")))
        idle-time (- (:idle second-read) (:idle first-read))
        total-time (- (apply + (vals second-read)) (apply + (vals first-read)))
        actually-working (- total-time idle-time)]
    (* 100.0 (/ actually-working total-time))))

(defn collect-metrics
  []
  {:timestamp (get-timestamp),
   :cpu-percent (get-cpu-percent),
   :memory-percent (get-memory-usage),
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
   :interval {:coerce :long, :default 1, :desc "Sampling interval in seconds"},
   :output {:default "power_metrics.csv", :desc "Output file path"}})

(defn -main [& args] (monitor (cli/parse-opts args {:spec cli-options})))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

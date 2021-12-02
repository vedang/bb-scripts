(ns me.vedang.scripts.components
  "A script to find all the components that you should deploy your code
  to. A 'Component' is any namespace with a -main function defined in
  it."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as cs]
            [clojure.tools.cli :refer [parse-opts]]))


(def cli-options
  [["-t" "--timeout TIMEOUT"
    "Program times out in TIMEOUT seconds"
    :default 300
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 59 % 601) "Must be between 1 to 10 minutes"]]
   ["-e" "--exclude EXCLUDE-DIR"
    "Directory to exclude in the search. Pass option multiple times to exclude multiple dirs."
    :multi true
    :default ["test/" "qa/"]
    :update-fn conj]
   ["-h" "--help"]])

(defn usage
  [options-summary]
  (->> ["List all the release components that you should deploy your code to."
        ""
        "Usage: components [options] C1 C2"
        ""
        "Options:"
        options-summary
        ""
        "C1 and C2 are Git SHAs / Refs / Tags representing the changed code."
        "The default value for C1 is HEAD and C2 is master"
        "Examples: "
        "   components    Calculate components for changes between HEAD and master"
        "   components release-branch master"
        "   components -t 60 -e test -e qa"]
       (cs/join \newline)))

(defn error-msg
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (cs/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-options)]
    (cond
      ;; help => exit OK with usage summary
      (:help options) {:exit-message (usage summary) :ok? true}
      ;; errors => exit with description of errors
      errors {:exit-message (error-msg errors)}
      ;; only two arguments at max allowed
      (> 3 (count arguments))
      {:opts (assoc options
                    :latest (or (first arguments) "HEAD")
                    :earliest (or (second arguments) "master"))}
      ;; else failed custom validation => exit with usage summary
      :else {:exit-message (usage summary)})))

(defn exit
  [status msg]
  (println msg)
  (System/exit status))

(defn input
  "Shells out to git and finds files we are interested in."
  [earliest latest]
  (if (zero? (:exit (shell/sh "which" "git")))
    (filter #(cs/ends-with? % ".clj")
            (cs/split
             (:out
              (shell/sh "git" "diff"
                       earliest latest
                       "--name-only"))
             #"\n"))
    []))

(defn print-components
  [{:keys [earliest latest timeout]}]
  ;; Just quit if the code does not wrap up in time.
  (future (Thread/sleep (* timeout 1000))
          (exit 1 (format "[components] Timeout! %s sec" timeout)))
  (let [files (input earliest latest)]
    (println (cs/join \newline files))))

(defn -main
  [& args]
  (let [{:keys [opts exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (print-components opts))))

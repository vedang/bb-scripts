(ns me.vedang.scripts.components
  "A script to find all the components that you should deploy your code
  to. A 'Component' is any namespace with a -main function defined in
  it."
  (:require [clojure.java.shell :as shell]
            [clojure.set :as cset]
            [clojure.string :as cs]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.tools.namespace.dependency :as dep]
            [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.track :as track])
  (:import java.io.File))


(def cli-options
  [["-e" "--exclude-paths EXCLUDE-DIR"
    "Directory to exclude in the search. Pass option multiple times to exclude multiple dirs."
    :multi true
    :default ["test/" "qa/"]
    :update-fn conj]
   ["-s" "--source-paths SOURCE-DIR"
    "Directory where source files are stored. Pass option multiple times to include multiple dirs."
    :multi true
    :default ["src/"]
    :update-fn conj]
   ["-t" "--timeout TIMEOUT"
    "Program times out in TIMEOUT seconds."
    :default 60
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 9 % 121) "Must be between 10 seconds to 2 minutes"]]
   ["-v" "--verbose"
    "Prints out the actual files due to which a component is selected."]
   ["-h" "--help"
    "Prints out this message."]])

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
        "   components                       # Calculate components for changes between HEAD and master"
        "   components release-branch master # Calculate components for changes between release-branch and master "
        "   components -t 60 -e test -e qa   # Changes between HEAD and master, but ignore changes in test/ and qa/ dirs. Abort if the run takes more than 60s."
        "   components -v                    # Print components as well as the specific files due to which the component was selected."]
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

(defn clj-files
  "Given a list of `files`, and paths to `exclude`, filter out the files
  and only return clj-files not on the exclude paths."
  [files exclude-paths]
  (let [exclude-pred (apply some-fn
                            (map (fn [p] (fn [s] (cs/starts-with? s p)))
                                 exclude-paths))]
    (transduce (comp (remove exclude-pred)
                     (filter #(cs/ends-with? % ".clj")))
               conj
               []
               files)))

(defn input
  "Shells out to git and finds files we are interested in."
  [earliest latest exclude-paths]
  (if (zero? (:exit (shell/sh "which" "git")))
    (-> (shell/sh "git" "diff" earliest latest "--name-only")
        :out
        (cs/split #"\n")
        (clj-files exclude-paths))
    []))

(defn namespace-dep-graph
  "Build a dependency graph of all the clj namespaces in the given
  `source-paths`."
  [source-paths]
  (->> source-paths
       (mapcat #(find/find-sources-in-dir (File. %) find/clj))
       (file/add-files (dep/graph))))

(defn files-or-dirs->namespaces
  "For the given set of files or dir-paths, return the namespaces of all
  the Clojure files."
  [files-or-dirs]
  (->> files-or-dirs
       (mapcat #(find/find-namespaces-in-dir (File. %)))
       set))

(defn ns->dependent-namespaces
  "Given a tools.namespace `graph`, a function `valid-ns?` to identify
  valid namespaces and a set of `ns-under-test`, return a mapping of
  `ns->dependent-namespaces` . This is the full set of all namespaces
  affected by any change in a `ns-under-test` namespace."
  ;; Note: `dep/transitive-dependents-set` can return all dependents
  ;; in one call, but getting data per namespace helps debugging /
  ;; understanding why a particular ns was chosen
  [graph valid-ns? ns-under-test]
  (reduce (fn [acc n]
            (->> n
                 (dep/transitive-dependents (::track/deps graph))
                 (filter valid-ns?)
                 set
                 (assoc acc n)))
          {}
          ns-under-test))

(defn namespace->file
  "Given a tools.namespace `graph`, return a mapping of
  `namespace->file` for all entries in the graph."
  [graph]
  (reduce-kv (fn [acc k v] (assoc acc v k))
             {}
             (::file/filemap graph)))

(defn output
  "Given data generated by `ns->dependent-namespaces`, the list of
  `components` and the verbosity flag, format the output of the
  program."
  [ns->dependent-nss components verbose?]
  (if verbose?
    (let [ns+deps (map (fn [c]
                         (let [v (reduce-kv (fn [out n deps]
                                              (if (deps c) (conj out n) out))
                                            []
                                            ns->dependent-nss)]
                           [c v]))
                       components)
          changed-nss (keys ns->dependent-nss)
          ns+deps-str (cs/join \newline
                               (map (fn [[n ds]]
                                      (format "%s => impacted by changes in %s" n ds))
                                    ns+deps))]
      (->> ["Namespaces changed between these two points"
            ""
            (cs/join \newline changed-nss)
            ""
            "Components impacted by these code changes:"
            ""
            ns+deps-str]
           (cs/join \newline)))
    (str "Components impacted by these code changes:\n\n"
         (cs/join \newline components))))

(let [main-re #"defn -main"]
  (defn get-components
    "Given data generated by `ns->dependent-namespaces` and data generated
    by `namespace->file`, return all the component namespaces as a list."
    [ns->dependent-nss ns->file]
    (filterv (comp (partial re-seq main-re) slurp ns->file)
             (apply cset/union (vals ns->dependent-nss)))))

(defn print-components
  [{:keys [earliest latest timeout exclude-paths source-paths verbose]}]
  ;; Just quit if the code does not wrap up in time.
  (future (Thread/sleep (* timeout 1000))
          (exit 1 (format "[components] Timeout! %s sec" timeout)))
  (let [files (input earliest latest exclude-paths)]
    (when (seq files)
      (let [global-dep-graph (namespace-dep-graph source-paths)
            global-excluded-nss (files-or-dirs->namespaces exclude-paths)
            changed-nss (files-or-dirs->namespaces files)
            dependent-nss (ns->dependent-namespaces global-dep-graph
                                                    (complement global-excluded-nss)
                                                    changed-nss)
            ns->file (namespace->file global-dep-graph)
            components (get-components dependent-nss ns->file)]
        (println (output dependent-nss components verbose))))))

(defn -main
  [& args]
  (let [{:keys [opts exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (print-components opts))))

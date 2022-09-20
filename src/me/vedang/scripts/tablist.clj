(ns me.vedang.scripts.tablist
  "A script to list all the open tabs in your browser and covert them to
  plain text in Org-Mode format. This script uses TabFS to get
  information of all the open tabs from the browser. To install TabFS,
  please refer to https://omar.website/tabfs/#setup"
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [cljc.java-time.local-date :as cjld]
   [cljc.java-time.local-date-time :as cjldt]
   [cljc.java-time.format.date-time-formatter :as cjtfdf]
   [clojure.string :as cs]))

(def org-mode-schedule-date-pattern
  (cjtfdf/of-pattern "<yyyy-MM-dd E>"))

(defn schedule-date
  "Coerces a date string, as entered on the CLI, to <yyyy-MM-dd E>
  format. This is the format that Org-Mode uses for Scheduled dates."
  [date-str]
  (-> date-str cjld/parse (cjld/format org-mode-schedule-date-pattern)))

(def org-mode-inactive-date-time-pattern
  (cjtfdf/of-pattern "yyyy-MM-dd E HH:mm"))

(defn inactive-date-time
  "Return a date in [yyyy-MM-dd E HH:mm] format, because this is the format
  that Org-Mode uses for inactive dates."
  [date]
  (str "[" (cjldt/format date org-mode-inactive-date-time-pattern) "]"))

(defn tabfs-mnt-error
  [dir]
  (cs/join \newline
           [(str "TabFS Mount directory not found at " dir)
            "Are you sure that you have installed TabFS correctly?"
            "Refer to https://omar.website/tabfs/#setup for instructions"]))

(def cli-options-spec
  {:tabfs-mnt-path
   {:desc "The path on the filesystem where TabFS is mounted."
    :alias :fs
    :validate {:pred fs/directory?
               :ex-msg (fn [m] (tabfs-mnt-error (:value m)))}
    :default-desc "*REQUIRED* TabFS mount path."
    :default "/Users/nejo/src/github/TabFS/fs/mnt"}
   :org-mode-heading-level
   {:desc "The heading level at which link headings will be stored in Org Mode."
    :coerce :int
    :validate {:pred pos?
               :ex-msg (fn [m] (str "Not a positive number: " (:value m)))}
    :alias :hl
    :default-desc "1"
    :default 1}
   :scheduled-on
   {:ref "<date>"
    :desc "Date on which to schedule reading this batch of links, in yyyy-MM-dd format."
    :coerce schedule-date
    :validate {:pred string? ;; assumes that coerce worked
               :ex-msg (fn [m] (str "Incorrect date format: " (:value m)))}
    :alias :s
    :default-desc "tomorrow"
    :default (cjld/format (cjld/plus-days (cjld/now) 1)
                          org-mode-schedule-date-pattern)}
   :delete-tabs
   {:ref "<del>"
    :desc "Boolean value to indicate if browser tabs should be closed"
    :coerce :boolean
    :alias :del
    :default-desc "false"
    :default false}})

(defn usage
  []
  (->> ["Convert all open tabs to plain text Org Mode format. Optionally, close the open tabs."
       ""
       "Usage: bb-plain-text-tabs [options]"
       ""
       "Options:"
       (cli/format-opts {:spec cli-options-spec
                         :order [:tabfs-mnt-path
                                 :org-mode-heading-level
                                 :scheduled-on
                                 :delete-tabs]})
       ""
       "Examples: "
       "   bb-plain-text-tabs -fs <TabFS Mount Path> # Convert Open Tabs to Plain Text"
       "   bb-plain-text-tabs -fs <TabFS Mount Path> --delete-tabs # Delete tabs and print plain text"
       "   bb-plain-text-tabs -fs <TabFS Mount Path> | tee tabs.org # Capture Plain Text output to file"
       ""]
      (cs/join \newline)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [cli-opts (cli/parse-opts args {:spec cli-options-spec})
        ;; Note: /by-id/ does not allow deletions on it, which stops
        ;; us from closing the tab. Hence we use /by-title/
        ;; This is probably a bug in TabFS, needs more investigation.
        tab-dir (str (:tabfs-mnt-path cli-opts) "/tabs/by-title")
        opts (merge cli-opts
                    {:tab-dir tab-dir
                     :captured-on (inactive-date-time (cjldt/now))})]
    (cond
      ;; help => exit OK with usage summary
      (:help opts) {:exit-message (usage) :ok? true}
      ;; tab-dir must be an accessible directory
      (not (fs/directory? tab-dir))
      {:exit-message (tabfs-mnt-error tab-dir)}
      ;; else all validation passed, go ahead
      :else {:ok? true :opts opts})))

(defn build-tab-info
  "Collects the title and URL for all open tabs using TabFS."
  [opts]
  (map (fn [t]
         {:title (first (fs/read-all-lines (str t "/title.txt")))
          :url (first (fs/read-all-lines (str t "/url.txt")))})
       (fs/list-dir (:tab-dir opts))))

(def plain-text-template
  "The template for creating the org-mode plain-text entry. Placeholders
are for the following items (in order):
  1. Org-Mode Heading level: default value is 1.
  2. URL of the link to the stored.
  3. Title of the link to be stored.
  4. Date when the Tab is scheduled for reading: default is tomorrow.
  5. Date when the Tab was captured."
  "%s TODO [[%s][%s]]\nSCHEDULED: %s\n%s")

(defn tabs->text
  "Convert the list of open tabs to a wall of text that can be stored in
  Plain-Text."
  [opts]
  (->> (build-tab-info opts)
      (map (fn [t]
             (format plain-text-template
                     (apply str (repeat (:org-mode-heading-level opts) "*"))
                     (:url t)
                     (:title t)
                     (:scheduled-on opts)
                     (:captured-on opts))))
      (cs/join \newline)))

(defn exit
  [status msg]
  (println msg)
  (System/exit status))

(defn -main
  [& args]
  (let [{:keys [opts exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [text-block (tabs->text opts)]
        (when (:delete-tabs opts)
          (try (fs/delete-tree (:tab-dir opts))
               ;; A java.nio.file.AccessDeniedException error is thrown
               ;; once all tabs are closed, probably as an artifact of
               ;; macFuse. I haven't looked into it.
               (catch Exception _)))
        (println text-block)))))

(comment
  (let [{:keys [opts exit-message ok?]} (validate-args *command-line-args*)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [text-block (tabs->text opts)]
        (when (:delete-tabs opts)
          (try (fs/delete-tree (:tab-dir opts))
               ;; A java.nio.file.AccessDeniedException error is thrown
               ;; once all tabs are closed, probably as an artifact of
               ;; macFuse. I haven't looked into it.
               (catch Exception _)))
        (println text-block)))))

(require '[cheshire.core :as json]
         '[clojure.string :refer [join split]]
         '[clojure.walk :refer [postwalk]]
         '[nio2.core :as nio])


(defn display-help
  []
  (println (join
            \newline
            [""
             "Tool for fixing file paths in coveralls report files."
             ""
             "Usage: lein exec -p <SCRIPT> <REPO_ROOT_DIR> <REPORT_FILE_PATH>"
             ""
             "TRAVIS CI EXAMPLE:"
             "  cd \"${TRAVIS_BUILD_DIR}/environments/development\""
             "  lein cloverage -o cov --coveralls"
             (str "  lein exec -p scripts/coveralls/fix-paths.clj"
                  " \"${TRAVIS_BUILD_DIR}\" cov/coveralls.json")
             ""]))
  (System/exit 0))


(def str->path (partial nio/path (nio/default-fs)))


(defn parse-args
  []
  (let [[me root report & rest] *command-line-args*
        root-dir-path (str->path root)
        report-file-path (str->path report)]
    (if-let [err-msg
             (cond
               ;; help
               (contains? #{nil "-h" "--help"} root)
               (display-help)
               ;; dir check
               ((complement nio/file?) (str->path "project.clj"))
               "The script must be run from the project root directory."
               ;; Root directory
               (empty? root)
               "Provide repository root directory."
               ((complement nio/exists?) root-dir-path)
               "Root directory doesn't exist."
               ((complement nio/dir?) root-dir-path)
               "Root directory isn't a directory."
               ;; Report file
               (empty? report)
               "Provide path to the report file."
               ((complement nio/exists?) report-file-path)
               "Specified file doesn't exist."
               ((complement nio/readable?) report-file-path)
               "Specified file isn't readable."
               ((complement nio/writable?) report-file-path)
               "Specified file isn't writable."
               ((complement nio/file?) report-file-path)
               "Specified file isn't file."
               ;; Rest arguments
               (seq rest)
               (str "This script accepts only two arguments.\n"
                    "Extra arguments: " (str rest)))]
      (binding [*out* *err*]
        (println err-msg)
        (System/exit 2))
      {:report (->> report-file-path
                    (nio/read-all-lines)
                    (join \newline)
                    (json/parse-string))
       :root-path root-dir-path
       :file-path report-file-path})))


(defn fix-path
  [root path]
  (->> path
       (str "src/")
       (str->path)
       (nio/absolute)
       (nio/file)
       (.getCanonicalPath)
       (str)))


(let [{:keys [report root-path file-path]} (parse-args)
      path-key "name"]
  (->> report
       (postwalk #(if (and (map-entry? %)
                           (= path-key (first %)))
                    [(first %) (fix-path root-path (second %))]
                    %))
       (json/generate-string)
       (vector)
       (nio/write-lines file-path)))


(prn "Done!")

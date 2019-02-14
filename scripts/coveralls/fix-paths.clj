(require
 '[cheshire.core :as json]
 '[clojure.java.io :as io]
 '[clojure.string :as str]
 '[clojure.walk :refer [postwalk]])


(defn display-help
  []
  (println (str/join
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


(defn parse-args
  []
  (let [[me root report & rest] *command-line-args*
        root-dir (io/as-file root)
        report-file (io/as-file report)]
    (if-let [err-msg
             (cond
               ;; help
               (contains? #{nil "-h" "--help"} root)
               (display-help)
               ;; dir check
               (not (.exists (io/as-file "project.clj")))
               "The script must be run from the project root directory."
               ;; Root directory
               (empty? root)
               "Provide repository root directory."
               (not (.exists root-dir))
               "Root directory doesn't exist."
               (not (.isDirectory root-dir))
               "Root directory isn't a directory."
               ;; Report file
               (empty? report)
               "Provide path to the report file."
               (not (.exists report-file))
               "Specified file doesn't exist."
               (.isDirectory report-file)
               "Specified file is a directory."
               ;; Rest arguments
               (seq rest)
               (str "This script accepts only two arguments.\n"
                    "Extra arguments: " (str rest)))]
      (binding [*out* *err*]
        (println err-msg)
        (System/exit 2))
      {:report (->> report-file
                    (slurp)
                    (json/parse-string))
       :root root-dir
       :file report-file})))


(defn fix-path
  [^java.io.File root ^java.io.File path]
  (let [can-path (->> path
                      (str "src/")
                      (io/as-file)
                      (.getCanonicalPath))
        can-root (.getCanonicalPath root)]
    (if (.exists (io/as-file (str can-root path)))
      path
      (if (str/starts-with? can-path can-root)
        (str/replace can-path can-root "")
        (throw (ex-info "File isn't inside the root dir"
                        {:file can-path :root can-root}))))))


(let [{:keys [report root file]} (parse-args)
      path-key "name"]
  (->> report
       (postwalk #(if (and (map-entry? %)
                           (= path-key (first %)))
                    [(first %) (fix-path root (second %))]
                    %))
       (json/generate-string)
       (spit file)))


(prn "Done!")

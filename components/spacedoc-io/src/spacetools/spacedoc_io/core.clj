 (ns spacetools.spacedoc-io.core
  "File-system I/O module.
  Functions that can fail return `cats.monad.exception`.
  Actually I/O wrapped in `io!`."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.interface :as sdu]))


(defn-spec file? boolean?
  [file any?]
  (instance? java.io.File file))


(defn-spec error? boolean?
  [err any?]
  (instance? Throwable err))


(s/def ::file-or-string (s/or :file-path string? :file file?))


(defn-spec absolute file?
  [path ::file-or-string]
  (io! (.getCanonicalFile (.getAbsoluteFile (io/file path)))))


(defn-spec mkdir boolean?
  "Make directory tree.
  Returns true if actually created something."
  [path ::file-or-string]
  (io! (.mkdirs (io/file path))))


(defn-spec rebase-path file?
  [old-base ::file-or-string new-base ::file-or-string path ::file-or-string]
  (io!
   (let [a-ob (str (absolute old-base))
         a-nb (str (absolute new-base))
         a-p (str (absolute path))]
     (io/file (str/replace-first a-p a-ob a-nb)))))


(defn-spec *spit exc/exception?
  "Like `spit` but also creates parent directories.
  Output is wrapped in try monad."
  [path ::file-or-string content any?]
  (io!
   (exc/try-or-recover
    (let [a-path (absolute path)
          parent-dir (.getParent (io/file a-path))]
      (mkdir parent-dir)
      (spit path content)
      a-path)
    (fn [^Exception err]
      (exc/failure
       (ex-info "Can't write file" {:path path}))))))


(defn-spec sdn-file? boolean?
  [path ::file-or-string]
  (io!
   (and (.isFile (io/file path))
        (re-matches #"(?i).*\.sdn$" (str path))
        true)))


(defn-spec directory? boolean?
  [path ::file-or-string]
  (io!
   (when-let [f (io/file path)]
     (.isDirectory f))))


(defn-spec *sdn-fps-in-dir exc/exception?
  [input-dir-path ::file-or-string]
  (io!
   (exc/try-on
    (if-not (directory? input-dir-path)
      (exc/failure (ex-info "File isn't a directory"
                            {:file input-dir-path}))
      (into #{}
            (comp
             (map #(.getCanonicalPath (io/file %)))
             (map str)
             (filter sdn-file?))
            (file-seq (io/file input-dir-path)))))))


(defn-spec *fp->sdn exc/exception?
  "Read and validate .SDN file."
  ([path ::file-or-string]
   (*fp->sdn :spacetools.spacedoc.node/root path))
  ([root-node-spec s/spec? path ::file-or-string]
   (io!
    (exc/try-or-recover
     (with-open [input (->> path
                            (io/file)
                            (io/reader)
                            (java.io.PushbackReader.))]
       (let [[obj fin] (repeatedly 2 (partial edn/read {:eof :fin} input))]
         (cond
           (not= :fin fin)
           (throw (Exception. ".SDN file should contain single root form."))
           (not= :root (:tag obj))
           (throw (Exception. "Non-root top level node in .SDN file."))
           ((complement s/valid?) root-node-spec obj)
           (throw (ex-info "Validation filed." (sdu/explain-deepest obj)))
           :else obj)))
     (fn [^Exception err]
       (exc/failure
        (ex-info (.getMessage err) (if-let [ed (ex-data err)]
                                     (assoc ed :file path)
                                     {:file path}))))))))


(defn-spec println-err nat-int?
  [& msg (s/* string?)]
  (io!
   (binding [*out* *err*]
     (println (str/join msg))
     2)))


(defn-spec println-ok nat-int?
  [& msg (s/* string?)]
  (io!
   (println (str/join msg))
   0))


(defn-spec err->msg string?
  [^Throwable err error?]
  (let [{:keys [cause data]} (Throwable->map err)]
    (str/join \newline
              (interleave
               (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                    ["Cause" "File" "Data"])
               [cause
                (or (:file data) "<none>")
                (or (:problems data) (seq data) "<none>")]))))


(defn-spec try-m->output nil?
  "Prints output to stderr or stdout and `System/exit` with code 0 or 2"
  [*output exc/exception?]
  (io!
   (System/exit
    (let [output (m/extract *output)]
      (if (exc/failure? *output)
        (println-err
         (format "Error:\n%s\nRun with \"-h\" for usage" (err->msg output)))
        (println-ok output))))))


(defn-spec edn-file? boolean?
  [path ::file-or-string]
  (io!
   (and (.isFile (io/file path))
        (re-matches #"(?i).*\.edn$" (str path))
        true)))


(defn-spec *slurp-cfg-overrides exc/exception?
  [overrides-fp ::file-or-string]
  (io!
   (exc/try-or-recover
    (when (edn-file? overrides-fp)
      (let [cfg-ovr (-> overrides-fp slurp edn/read-string)]
        (if (sdu/valid-overrides? cfg-ovr)
          cfg-ovr
          (throw
           (ex-info
            "Invalid overrides"
            {:explanation (s/explain-data
                           :spacetools.spacedoc.config/overriding-configs
                           cfg-ovr)})))))
    (fn [^Exception err]
      (exc/failure
       (ex-info "Can't read configuration overrides file"
                {:path overrides-fp :error err}))))))

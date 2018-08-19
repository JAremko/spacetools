(ns spacedoc.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [cats.monad.exception :as exc]
            [clojure.tools.cli :refer [parse-opts]]
            [spacedoc.data :as data]
            [cats.core :as m]
            [clojure.core.reducers :as r]
            [clojure.string :refer [replace-first]]))


(defn absolute
  [path]
  (io! (.getCanonicalFile (.getAbsoluteFile (io/file path)))))


(defn mkdir
  "Make directory tree.
  Returns true if actually created something."
  [path]
  (io! (.mkdirs (io/file path))))


(defn rebase-path
  [old-base new-base path]
  (io!
   (let [a-ob (str (absolute old-base))
         a-nb (str (absolute new-base))
         a-p (str (absolute path))]
     (io/file (replace-first a-p a-ob a-nb)))))


(defn *spit
  "Like `spit` but also creates parent directories.
  Output is wrapped in try monad."
  [path content]
  (io!
   (exc/try-or-recover
    (let [a-path (absolute path)
          parent-dir (.getParent a-path)]
      (mkdir parent-dir)
      (spit path content)
      a-path)
    (fn [^Exception err]
      (exc/failure
       (ex-info "Can't write file" {:path path :err err}))))))


(defn sdn-file?
  [path]
  (io! (and (.isFile (io/file path))
            (re-matches #"(?i).*\.sdn$" (str path))
            true)))


(defn directory?
  [path]
  (io!
   (when-let [f (io/file path)]
     (.isDirectory f))))


(defn *sdn-fps-in-dir
  [input-dir-path]
  (io!
   (exc/try-on (if-not (directory? input-dir-path)
                 (exc/failure (ex-info "File isn't a directory"
                                       {:file input-dir-path}))
                 (into #{}
                       (comp
                        (map #(.getCanonicalPath (io/file %)))
                        (map str)
                        (filter sdn-file?))
                       (file-seq (io/file input-dir-path)))))))


(defn *fp->spacedoc
  "Read and validate Spacedoc END file."
  ([path]
   (*fp->spacedoc :spacedoc.data/root path))
  ([root-node-spec path]
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
           (not (s/valid? root-node-spec obj))
           (throw (ex-info "Validation filed." (data/explain-deepest obj)))
           :else obj)))
     (fn [^Exception err]
       (exc/failure
        (ex-info (.getMessage err) (if-let [ed (ex-data err)]
                                     (assoc ed :file path)
                                     {:file path}))))))))


(defn exit-err
  "`println` MSG to STDERR and exit with code 2."
  [msg]
  (io!
   (binding [*out* *err*]
     (println msg)
     (System/exit 2))))


(defn exit-success
  "`println` MSG to STDOUT and exit with code 0."
  [msg]
  (io!
   (println msg)
   (System/exit 0)))

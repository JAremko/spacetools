(ns spacedoc.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [cats.monad.exception :as exc]
            #_ [lacij.view.graphview :as gv]
            [clojure.tools.cli :refer [parse-opts]]
            [spacedoc.data :as data]
            [cats.core :as m]
            [clojure.core.reducers :as r]))


(defn sdn-file?
  [file-path]
  (io! (and (.isFile (io/file file-path))
            (re-matches #"(?i).*\.sdn$" (str file-path))
            true)))


(defn directory?
  [file-path]
  (io!
   (when-let [f (io/file file-path)]
     (.isDirectory f))))


(defn parent-dir-writable?
  [file-path]
  (io!
   (some-> file-path
           (io/file)
           (.getCanonicalPath)
           (io/file)
           (.getParentFile)
           (.exists))))


(defn sdn-fps-in-dir
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


(defn fp->spacedoc
  "Read and validate Spacedoc END file."
  ([file-path]
   (fp->spacedoc :spacedoc.data/root file-path))
  ([root-node-spec file-path]
   (io!
    (exc/try-on
     (with-open [input (->> file-path
                            (io/file)
                            (io/reader)
                            (java.io.PushbackReader.))]
       (let [[obj fin] (repeatedly 2 (partial edn/read {:eof :fin} input))]
         (if-let [e (cond
                      (not= :fin fin)
                      [{} (Exception. "File should contain single root form.")]
                      (not (s/valid? root-node-spec obj))
                      [(data/explain-deepest obj) "Validation filed."])]
           (apply exc/failure (assoc-in e [0 :file] file-path))
           obj)))))))


#_ (defn export-graph-svg
     [file-path graph]
     (exc/try-on
      (io!  (gv/export graph file-path :indent "yes")
            (format "Successfully exported: \"%s\"" file-path))))


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

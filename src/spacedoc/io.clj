(ns spacedoc.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [cats.monad.exception :as exc]
            [lacij.view.graphview :as gv]
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
     (and (.exists f)
          (.isDirectory f)))))


(defn parent-dir-writable?
  [file-path]
  (io!
   (and (not (directory? file-path))
        (some-> file-path
                (io/file)
                (.getParentFile)
                (.exists)))))


(defn-  uberjar-work-dir
  []
  (io!
   (-> (class *ns*)
       .getProtectionDomain
       .getCodeSource
       .getLocation
       .getPath
       io/file
       .getParent)))


(defn- lein-work-dir
  []
  (io!
   (System/getProperty "user.dir")))


(defn default-input-dir-exc
  []
  (io!
   (exc/try-on
    (let [d (uberjar-work-dir)
          lein (lein-work-dir)
          etools "emacs-tools"
          u-r (str d "/" etools)
          u-t (str d "/../../" etools)
          t-d "/emacs-tools/export/target"]
      (if-let
          [root-dir
           (cond
             (directory? u-r) d
             (directory? u-t) (.getCanonicalPath (io/file (str d "/../../")))
             (directory? (str lein "/" etools)) lein)]
        (let [target-dir (str root-dir t-d)]
          (if (directory? target-dir)
            target-dir
            (exc/failure (ex-info "Target isn't a directory."
                                  {:target target-dir}))))
        (exc/failure (Exception. "Can't locate Spacedoc directory.")))))))


(defn input-dir->sdn-fps-exc
  [input-dir-path]
  (io!
   (exc/try-on (eduction
                (map #(do (println "1") %))
                (map #(.getCanonicalPath %))
                (map str)
                (filter sdn-file?)
                (file-seq (io/file input-dir-path))))))


(defn fp->spacedoc-exc
  "Read and validate Spacedoc END file."
  ([file-path]
   (fp->spacedoc-exc :spacedoc.data/root file-path))
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


;; (defn input-dir->spacedocs-exc
;;   [input-dir]
;;   (io!
;;    (exc/try-on
;;     (m/alet [idir (if input-dir
;;                     (exc/success input-dir)
;;                     (default-input-dir-exc))
;;              sdn-fps (input-dir->sdn-fps-exc idir)]
;;             (eduction (map fp->spacedoc-exc) sdn-fps)))))


(defn export-graph-svg
  [file-path graph]
  (io! (gv/export graph file-path :indent "yes")))


(defn exit-err
  "`println` ERR to STDERR and exit with code 2."
  [err]
  (io!
   (binding [*out* *err*]
     (println err)
     (System/exit 2))))

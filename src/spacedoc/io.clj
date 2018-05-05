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


(defn edn-file?
  [file-path]
  (io! (and (.isFile (io/file file-path))
            (re-matches #"(?i).*\.edn$" (str file-path)))))


(defn directory?
  [file-path]
  (io!
   (when-let [f (io/file file-path)]
     (and (.exists f)
          (.isDirectory f)))))


(defn-  uberjar-work-dir
  []
  (-> (class *ns*)
      .getProtectionDomain
      .getCodeSource
      .getLocation
      .getPath
      io/file
      .getParent))


(defn- lein-work-dir
  []
  (System/getProperty "user.dir"))


(defn default-input-dir-m
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


(defn input-dir->edn-fps-m
  [input-dir-path]
  (io!
   (exc/try-on (eduction
                (map #(.getCanonicalPath %))
                (map str)
                (filter edn-file?)
                (file-seq (io/file input-dir-path))))))


(defn fp->spacedoc-m
  "Read and validate Spacedoc END file."
  ([file-path]
   (fp->spacedoc-m :spacedoc.data/root file-path))
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
                      [(Exception. "File should contain single root form")]
                      (not (s/valid? root-node-spec obj))
                      [(data/explain-deepest obj) "Validation filed"])]
           (apply exc/failure e) obj)))))))


(defn args->spacedocs-m
  [args ops]
  (io!
   (exc/try-on
    (let [{{:keys [input]} :options errors :errors} (parse-opts args ops)]
      (if errors
        (exc/failure (ex-info "Bad args" {:errors errors}))
        (m/alet [input-dir (if input
                             (exc/success input)
                             (default-input-dir-m))
                 edn-fps (input-dir->edn-fps-m input-dir)]
                (eduction (map fp->spacedoc-m) edn-fps)))))))


(defn export-graph-svg
  [file-path graph]
  (io! (gv/export graph file-path :indent "yes")))

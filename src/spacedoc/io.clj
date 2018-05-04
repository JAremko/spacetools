(ns spacedoc.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [cats.monad.exception :as exc]
            [lacij.view.graphview :as gv]
            [spacedoc.data :as data]))


(defn directory?
  [file-path]
  (io!
   (when-let [f (io/file file-path)]
     (and (.exists f)
          (.isDirectory f)))))


(defn root-dir
  []
  (io!
   (let [d (-> (class *ns*)
               .getProtectionDomain
               .getCodeSource
               .getLocation
               .getPath
               io/file
               .getParent)
         from-root (str d "./emacs-tools")
         from-target (str d "/../../emacs-tools")]
     (cond
       (directory? from-root) d
       (directory? from-target) (.getCanonicalPath (io/file (str d "/../../")))
       :else (System/getProperty "user.dir")))))


(defn fp->*spacedoc-m
  "Read and validate Spacedoc END file."
  ([file]
   (fp->*spacedoc-m :spacedoc.data/root file))
  ([root-node-spec file]
   (io!
    (exc/try-on
     (with-open [input (->> file
                            (clojure.java.io/reader)
                            (java.io.PushbackReader.))]
       (let [[obj fin] (repeatedly 2 (partial edn/read {:eof :fin} input))]
         (if-let [e (cond
                      (not= :fin fin)
                      [(Exception. "File should contain single top level form")]
                      (not (s/valid? root-node-spec obj))
                      [(data/explain-deepest obj) "Validation filed:"])]
           (apply exc/failure e) obj)))))))


(defn edn-file?
  [file]
  (io! (and (.isFile file) (re-matches #"(?i).*\.edn$" (str file)))))


(defn edn-files-in-dir
  [root-dir]
  (io! (sequence (filter edn-file?) (file-seq root-dir))))


(defn export-graph-svg
  [file graph]
  (io! (gv/export graph file :indent "yes")))

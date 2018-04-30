(ns spacedoc.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spacedoc.data :as data]))


(defn fp->spacedoc
  "Read Spacedoc END file."
  ([file]
   (fp->spacedoc :spacedoc.data/root file))
  ([root-node-spec file]
   (io!
    (with-open [input (->> file
                           (clojure.java.io/reader)
                           (java.io.PushbackReader.))]
      (let [[obj fin] (repeatedly 2 (partial edn/read {:eof :fin} input))]
        (when-let [e (cond (not= :fin fin)
                           "File should contain single top level form"
                           (not (s/valid? root-node-spec obj))
                           (format
                            "Spec validation filed: (%s)"
                            (data/explain-str-deepest obj)))]
          (throw (Exception. e)))
        obj)))))


(defn edn-file?
  [file]
  (and (.isFile file)
       (re-matches #"(?i).*\.edn$" (str file))))


(defn edn-files-in-dir
  [root-dir]
  (sequence (filter edn-file?) (file-seq root-dir)))

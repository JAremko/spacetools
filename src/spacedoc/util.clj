(ns spacedoc.util
  (:gen-class)
  (:require [clojure.core.reducers :refer [fold]]
            [clojure.set :as set :refer [union]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spacedoc.data :as data]
            [spacedoc.conf :as conf]))


(defn explain-deepest
  "Validate each node in DOC using multi-spec MSPEC.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [mspec doc]
  (or (when-let [c (:children doc)]
        (reduce
         (fn [_ v]
           (when-let [fail (explain-deepest mspec v)]
             (reduced fail)))
         nil c))
      (when-not (s/valid? mspec doc)
        (s/explain-str mspec doc))))


(defn read-spacedoc-files
  "Read Spacedoc END files"
  ([file-path]
   (io!
    (try
      (with-open [input (->> file-path
                             (clojure.java.io/reader)
                             (java.io.PushbackReader.))]
        (let [objs (repeatedly (partial edn/read {:eof :fin} input))]
          (if (= :fin (second objs))
            (first objs)
            (throw
             (Exception.
              "Spacdoc file should contain single top level form")))))
      (catch Exception e
        (throw
         (Exception.
          (format "\"%s\" During parsing of \"%s\" file" e file-path)))))))
  ([file-path & fs]
   (fold
    conf/*n-threads*
    (fn
      ([xs x] (merge xs x))
      ([] '()))
    #(conj %1 (read-spacedoc-files %2))
    (conj fs file-path))))


(defn children-tags
  "Return tag set of all children of a node specified by :tag value TAG
  in IN-DOCS Spacemacs EDN documentation EDN structures."
  ([tag]
   (fn [xs]
     (->> xs
          (tree-seq :children :children)
          (filter #(= (:tag %) tag))
          (map :children)
          (flatten)
          (map :tag)
          (into #{}))))
  ([tag in-docs]
   (fold
    conf/*n-threads*
    (fn
      ([xs x] (union xs x))
      ([] #{}))
    #(union %1 ((children-tags tag) %2))
    in-docs)))

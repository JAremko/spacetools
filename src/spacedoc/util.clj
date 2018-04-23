(ns spacedoc.util
  (:require [clojure.core.reducers :refer [fold]]
            [clojure.set :as set :refer [union]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spacedoc.data :as data]
            [spacedoc.conf :as conf]))


(defn explain-str-deepest
  "Validate each node in DOC Spacedoc structure using multi-spec MSPEC.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [mspec doc]
  (or (when-let [c (:children doc)]
        (reduce
         (fn [_ v]
           (when-let [fail (explain-str-deepest mspec v)]
             (reduced fail)))
         nil c))
      (when-not (s/valid? mspec doc)
        (s/explain-str mspec doc))))


(defn read-files
  "Read Spacedoc END files"
  ([file-path]
   (io!
    (try
      (with-open [input (->> file-path
                             (clojure.java.io/reader)
                             (java.io.PushbackReader.))]
        (let [[obj fin] (repeatedly 2 (partial edn/read {:eof :fin} input))]
          (when-some [e (cond (not= :fin fin)
                              "File should contain single top level form"
                              (not (s/valid? :spacedoc.data/root obj))
                              (format
                               "Spec validation filed: (%s)"
                               (explain-str-deepest
                                :spacedoc.data/obj->spec
                                obj)))]
            (throw (Exception. e)))
          obj))
      (catch Exception e
        (throw
         (Exception.
          (format "\"%s\" During reading of \"%s\" file" e file-path)))))))
  ([file-path & fs]
   (fold
    conf/*n-threads*
    (fn
      ([xs x] (merge xs x))
      ([] '()))
    #(conj %1 (read-files %2))
    (conj fs file-path))))


(defn children-tags
  "Return tag set of all children of a node specified by :tag value TAG
  in IN-DOCS Spacedoc structures."
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

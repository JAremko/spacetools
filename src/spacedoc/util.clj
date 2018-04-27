(ns spacedoc.util
  (:require [clojure.core.reducers :refer [fold]]
            [clojure.set :as set :refer [union]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spacedoc.data :as data]
            [spacedoc.conf :as conf]))

(definline obj->spec
  [obj]
  (some->> obj
           (:tag)
           (data/all-tags)
           (name)
           (keyword "spacedoc.data")))


(defn explain-str-deepest
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node]
  (or (when-let [c (:children node)]
        (reduce
         (fn [_ v]
           (when-let [fail (explain-str-deepest v)]
             (reduced fail)))
         nil c))
      (when-not (s/valid? (obj->spec node) node)
        (s/explain-str (obj->spec node) node))))


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
                            (explain-str-deepest obj)))]
          (throw (Exception. e)))
        obj)))))


(defn node-graph
  "Return node graph of all nodes IN-DOCS Spacedoc collections."
  [in-docs]
  (fold conf/*n-threads*
        (fn
          ([m1 m2] (merge-with union m1 m2))
          ([] {}))
        #(merge-with union
                     %1
                     (reduce
                      (fn [tag->children-ts node]
                        (let [tag (:tag node)
                              children-ts (some->> node
                                                   :children
                                                   (map :tag)
                                                   (set))]
                          (update tag->children-ts
                                  tag
                                  union
                                  children-ts)))
                      {}
                      (tree-seq :children :children %2)))
        in-docs))

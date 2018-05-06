(ns spacedoc.data
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.string :refer [join]]
            [clojure.repl :refer [doc]]
            [clojure.spec.alpha :as s]))


(def ^:private doc-ns-str (str *ns*))


(alias 'cs 'clojure.spec.alpha)


(load "data_spec")


(def all-tags (remove #{:default} (keys (methods node->spec-k))))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn- fmt-problem
  [node problem]
  (join "\n" (assoc problem
                    :node-tag (:tag node)
                    :node-spec (s/describe (node->spec-k node)))))


(defn explain-deepest
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node]
  (or (first (sequence (keep (partial explain-deepest)) (:children node)))
      (when-let [p (::cs/problems (s/explain-data (node->spec-k node) node))]
        {:problems (r/reduce str (r/map (partial fmt-problem node) p))})))


(defn node-relations
  "Return mapping between nodes and children sets."
  ([root]
   (r/reduce
    (r/monoid (fn [m n] (update m (:tag n) union (children-tag-s n))) hash-map)
    (tree-seq :children :children root)))
  ([root & roots]
   (r/fold (r/monoid (partial merge-with union) hash-map)
           (r/map node-relations (list* root roots)))))

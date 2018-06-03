(ns spacedoc.data
  (:require [spacedoc.util :as util]
            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.string :refer [join]]
            [clojure.spec.alpha :as s]
            [spec-tools.core :as st]))


(def doc-ns-str (str *ns*))


(alias 'cs 'clojure.spec.alpha)


(load "data_spec")


(def all-tags (remove #{:default} (keys (methods node->spec-k))))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn- fmt-problem
  [node problem]
  (join \newline
        (assoc problem
               :node-tag (:tag node)
               :spec-form (s/form (st/get-spec (node->spec-k node))))))


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
    (tree-seq :children :children root))))


(defn node-relations-aggregate
  "Apply `node-relations` to ROOTS and union the outputs.."
  [roots]
  {:pre [(util/foldable? roots)]}
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map node-relations roots)))

(ns spacedoc.data
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set :refer [union]]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))


(load "data_spec")


(def all-tags #{:bold
                :center
                :kbd
                :example
                :fixed-width
                :headline
                :headline-level-1
                :headline-level-2
                :headline-level-3
                :headline-level-4
                :headline-level-5
                :headline-level-6
                :todo
                :description
                :body
                :italic
                :list-item
                :item-children
                :item-tag
                :keyword
                :line-break
                :org-file-path
                :embeddable-file-path
                :file-path
                :embeddable-web-link
                :web-link
                :paragraph
                :feature-list
                :plain-list
                :plain-text
                :quote
                :section
                :src
                :strike-through
                :subscript
                :superscript
                :table
                :table-cell
                :table-row
                :root
                :underline
                :verbatim
                :verse})


(def ^:private doc-ns *ns*)


(eval `(s/def ::valid-node ~all-tags))


(defn node->spec
  [node]
  (or (some->> node
               (:tag)
               (all-tags)
               (name)
               (keyword (str doc-ns)))
      ::valid-node))


(defn explain-deepest
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node]
  (or (some->> node
               :children
               (keep (partial explain-deepest))
               first)
      (when-not (s/valid? (node->spec node) node)
        (s/explain-data (node->spec node) node))))


(def children-tag-s (comp (partial into #{} (map :tag))
                       :children))


(defn node-graph
  "Return mapping between nodes and children sets."
  [root-node]
  (r/reduce
   (r/monoid (fn [m n] (update m (:tag n) union (children-tag-s n))) hash-map)
   (tree-seq :children :children root-node)))


(defn node-graph-aggregate
  "Return `node-graph` of all nodes IN-DOCS Spacedoc collections united."
  [in-docs]
  (r/fold
   (r/monoid (partial merge-with union) hash-map)
   (r/map node-graph in-docs)))

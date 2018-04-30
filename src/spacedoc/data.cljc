(ns spacedoc.data
  (:require [clojure.core.reducers :refer [fold monoid]]
            [clojure.set :as set :refer [union]]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spacedoc.data :as data]
            [spacedoc.conf :as conf]))


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


(defn node->spec
  [node]
  (->> node
       (:tag)
       (data/all-tags)
       (name)
       (keyword (str doc-ns))))


(defn explain-str-deepest
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node]
  (or (first (keep (partial explain-str-deepest) (:children node)))
      (when-not (s/valid? (node->spec node) node)
        (s/explain-str (node->spec node) node))))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn node-graph
  "Return node graph of all nodes IN-DOCS Spacedoc collections."
  [in-docs]
  (fold conf/*n-threads*
        (monoid (partial merge-with union) (constantly nil))
        (fn [m1 m2]
          (merge-with union m1 (reduce
                                #(update %1 (:tag %2) union (children-tag-s %2))
                                {}
                                (tree-seq :children :children m2))))
        in-docs))

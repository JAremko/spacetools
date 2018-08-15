(ns spacedoc.data
  (:require [spacedoc.util :as util]
            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))


(def doc-ns-str (str *ns*))


;;;; Generate `max-headline-depth` levels of headline nodes.
(def max-headline-depth 5)


(def seps  #{\! \? \: \; \) \} \, \. \- \\ \newline \space \tab})


(load "data_spec")


(def kinds {inline-container-tags :inline-container
            inline-leaf-tags :inline-leaf
            block-tags :block
            headline-tags :headline})


(defn tag->kind
  [tag]
  (some->> kinds
           (filter #((key %) tag))
           (first)
           (val)))


(def all-tags (remove #{:default} (keys (methods node->spec-k))))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn- fmt-problem
  [node problem]
  (str/join \newline
            (assoc problem
                   :node-tag (:tag node)
                   :spec-form (s/form (node->spec-k node)))))


(defn explain-deepest
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node]
  (or (first (sequence (keep (partial explain-deepest)) (:children node)))
      (when-let [p (:clojure.spec.alpha/problems
                    (s/explain-data (node->spec-k node) node))]
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


(defn- val->path-id-frag
  [hl-value]
  (str/replace hl-value #"[^\pL\pN\p{Pc}/-]" ""))


(defn fill-hl
  "Give Headline placeholder a proper tag and fill all necessary key-vals."
  ([{tag :tag value :value :as headline}]
   (assoc headline
          :tag (if (= :headline tag)
                 :headline-level-1
                 tag)
          :level 1
          :path-id (val->path-id-frag value)))
  ([{p-level :level p-path-id :path-id} {tag :tag value :value :as headline}]
   (let [hl-level (inc p-level)]
     (assoc headline
            :tag (if (= :headline tag)
                   (keyword (concat "headline-level-" hl-level))
                   tag)
            :level hl-level
            :path-id (concat p-path-id "/" (val->path-id-frag value))))))

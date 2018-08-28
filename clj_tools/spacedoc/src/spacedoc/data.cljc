(ns spacedoc.data
  (:require [spacedoc.util :as u]
            [spacedoc.data.node :as n]
            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))


(def seps  #{\! \? \: \; \( \) \{ \} \, \. \- \\ \newline \space \tab})


;;;; Headline stuff


(defn path-id?
  [val]
  (re-matches
   ;; forgive me Father for I have sinned
   #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
   val))


(defn hl-val->gh-id-base
  [hl-value]
  (str "#"
       (-> hl-value
           (str/replace " " "-")
           (str/lower-case)
           (str/replace #"[^\p{Nd}\p{L}\p{Pd}\p{Pc}]" ""))))


(defn hl-val->path-id-frag
  [hl-value]
  (-> hl-value
      (str/lower-case)
      (str/replace #"[^\p{Nd}\p{L}\p{Pd}]" " ")
      (str/trim)
      (str/replace #"\s+" "_")))


(defn fill-hl
  "Give Headline placeholder a proper tag and fill all necessary key-vals."
  ([{tag :tag value :value :as headline}]
   (assoc headline
          :level 1
          :path-id (hl-val->path-id-frag value)))
  ([{p-level :level p-path-id :path-id :as parent-headline}
    {tag :tag value :value :as headline}]
   (let [hl-level (inc p-level)]
     (assoc headline
            :level hl-level
            :path-id (str p-path-id "/" (hl-val->path-id-frag value))))))


;;;; Generic data related stuff


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn- fmt-problem
  [node problem]
  (str/join \newline
            (assoc problem
                   :node-tag (:tag node)
                   :spec-form (s/form (n/node->spec-k node)))))


(defn explain-deepest
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node]
  (or (first (sequence (keep (partial explain-deepest)) (:children node)))
      (when-let [p (:clojure.spec.alpha/problems
                    (s/explain-data (n/node->spec-k node) node))]
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
  {:pre [(u/foldable? roots)]}
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map node-relations roots)))

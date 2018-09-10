(ns spacedoc.data
  "General SDN manipulation facilities."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacedoc.util :as u]))


(def seps  #{\! \? \: \; \( \) \{ \} \, \. \- \\ \newline \space \tab})


(def link-type->prefix {:file "file:"
                        :http "http://"
                        :https "https://"
                        :custom-id "#"
                        :ftp "ftp://"})


(def max-headline-depth 5)


;;;; Generic stuff for SDN manipulation


(defmulti node->spec-k :tag)


(defn tag->spec-k
  [node-tag]
  (node->spec-k {:tag node-tag}))


(defn all-tags
  []
  (set (remove #{:default} (keys (methods node->spec-k)))))


(defn known-node?
  [tag]
  ((all-tags) tag))


(s/def :spacedoc.data.node/known-node known-node?)
(defmethod node->spec-k :default [_] ::known-node)


(s/def :spacedoc.data.node/node (s/multi-spec node->spec-k :tag))


(defn link->link-prefix
  [path]
  (->> (vals link-type->prefix)
       (filter (partial str/starts-with? path))
       (first)))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn- fmt-problem
  [node problem]
  {:pre [(map? node) (:tag node) (map? problem)]}
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
  {:pre [(map? node) (:tag node)]}
  (or (first (sequence (keep explain-deepest) (:children node)))
      (when-let [p (:clojure.spec.alpha/problems
                    (s/explain-data (node->spec-k node) node))]
        {:problems (r/reduce str (r/map (partial fmt-problem node) p))})))


(defn node-relations
  "Return mapping between nodes and children sets."
  [parent]
  {:pre [(map? parent) (:tag parent)]}
  (r/reduce
   (r/monoid (fn [m n] (update m (:tag n) union (children-tag-s n))) hash-map)
   (tree-seq :children :children parent)))


(defn node-relations-aggregate
  "Apply `node-relations` to PARENTS and union the outputs.."
  [parents]
  {:pre [(u/foldable? parents)]}
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map node-relations parents)))


;;;; Headline stuff

(defn path-id?
  [val]
  (and
   (string? val)
   (re-matches
    ;; forgive me Father for I have sinned
    #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
    val)))


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
   {:post [(s/valid? (node->spec-k %) %)]}
   (assoc headline
          :level 1
          :path-id (hl-val->path-id-frag value)))
  ([{p-level :level p-path-id :path-id :as parent-headline}
    {tag :tag value :value :as headline}]
   {:post [(s/valid? (node->spec-k %) %)]}
   (let [hl-level (inc p-level)]
     (assoc headline
            :level hl-level
            :path-id (str p-path-id "/" (hl-val->path-id-frag value))))))

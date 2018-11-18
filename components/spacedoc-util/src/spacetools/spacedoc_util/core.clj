(ns spacetools.spacedoc-util.core
  "SDN manipulation utilities."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]))


(def seps
  #{\! \? \: \' \( \) \; \{ \} \, \. \\ \“ \‘ \’ \” \newline \space \tab})

(def seps-right (disj seps \) \” \’))

(def seps-left  (disj seps \( \“ \‘))

(def link-type->prefix {:file "file:"
                        :http "http://"
                        :https "https://"
                        :custom-id "#"
                        :ftp "ftp://"})

(def link-types (-> link-type->prefix keys set))

(def max-headline-depth 5)

(def *node-tag->spek-k (atom {}))


;;;; Generic stuff for SDN manipulation


(defn register-node!
  [tag spec-k]
  (swap! *node-tag->spek-k assoc tag spec-k))


(defn-spec node->spec-k (s/nilable qualified-keyword?)
  [node (s/map-of keyword? any?)]
  (@*node-tag->spek-k (:tag node)))


(defn-spec tag->spec-k qualified-keyword?
  [node-tag keyword?]
  (node->spec-k {:tag node-tag}))


(defn-spec all-tags (s/coll-of keyword? :kind set?)
  []
  (set (keys @*node-tag->spek-k)))


(defn-spec known-node? (s/nilable keyword?)
  [tag keyword?]
  ((all-tags) tag))


(s/def :spacetools.spacedoc.node/known-node known-node?)


(defn link->link-prefix
  [path]
  (->> (vals link-type->prefix)
       (filter (partial str/starts-with? path))
       (first)))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn fmt-problem
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


(defn relation
  "Return mapping between nodes and children sets."
  [parent]
  {:pre [(map? parent) (:tag parent)]}
  (r/reduce
   (r/monoid (fn [m n] (update m (:tag n) union (children-tag-s n))) hash-map)
   (tree-seq :children :children parent)))


(defn relations
  "Apply `relation` to PARENTS and `union` the outputs.."
  [parents]
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map relation parents)))


;;;; Table stuff

(defn same-row-length?
  "Returns true if each row of the table has same amount of cells."
  [table-children]
  {:pre [(every? #(#{:table-row} (:tag %)) table-children)]}
  (let [t-c (remove #(#{:rule} (:type %)) table-children)]
    (or (empty? t-c)
        (apply = (map #(count (:children %)) t-c)))))

#_ (same-row-length?
    [{:tag :table-row
      :type :rule
      :children [{:tag :table-cell :children [{:tag :text :value "s"}]}
                 {:tag :table-cell :children [{:tag :text :value "s"}]}]}
     {:tag :table-row
      :type :rule
      :children [{:tag :table-cell :children [{:tag :text :value "s"}]}]}])


;;;; Headline stuff

(defn hl-val->gh-id-base
  [hl-value]
  {:pre [((complement str/blank?) hl-value)]}
  (str "#"
       (-> hl-value
           (str/replace " " "-")
           (str/lower-case)
           (str/replace #"[^\p{Nd}\p{L}\p{Pd}\p{Pc}]" ""))))


(defn hl-val->path-id-frag
  [hl-value]
  {:pre [(string? hl-value)]}
  (-> hl-value
      (str/lower-case)
      (str/replace #"[^\p{Nd}\p{L}\p{Pd}]" " ")
      (str/trim)
      (str/replace #"\s+" "_")))


(defn path-id?
  [val]
  (and
   (string? val)
   (re-matches
    ;; forgive me Father for I have sinned
    #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
    val)))


(s/def ::filled-headline
  (s/keys :req-un [:spacetools.spacedoc.node.headline/value
                   :spacetools.spacedoc.node.headline/children
                   :spacetools.spacedoc.node.headline/level
                   :spacetools.spacedoc.node.headline/path-id]))


(defn fill-hl
  "Give Headline placeholder a proper tag and fill all necessary key-vals."
  ([{tag :tag value :value :as headline}]
   (assoc headline
          :level 1
          :path-id (hl-val->path-id-frag value)))
  ([{p-tag :tag p-level :level p-path-id :path-id :as parent-headline}
    {tag :tag value :value :as headline}]
   (let [hl-level (inc p-level)]
     (assoc headline
            :level hl-level
            :path-id (str p-path-id "/" (hl-val->path-id-frag value))))))


(defn up-tags
  "Update #+TAGS `:spacetools.spacedoc.node/key-word` of the ROOT-NODE.
  SPACEROOT is the root directory of Spacemacs and FILE is the exported file
  name. they are used for creating basic tags if non is present."
  [spaceroot file root-node]
  root-node)

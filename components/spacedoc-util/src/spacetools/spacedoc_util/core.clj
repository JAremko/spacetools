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

(s/def ::spec-problem (s/keys :req [:clojure.spec.alpha/problems
                                    :clojure.spec.alpha/spec
                                    :clojure.spec.alpha/value]))


;;;; Generic stuff for SDN manipulation


(defn-spec non-blank-string? boolean?
  [s any?]
  (and (string? s)
       ((complement str/blank?) s)))


(defn-spec node? boolean?
  [node any?]
  (and (:tag node)
       (s/valid? (s/map-of keyword? any?) node)))


(defn-spec register-node! node?
  [tag keyword? spec-k qualified-keyword?]
  (swap! *node-tag->spek-k assoc tag spec-k))


(defn-spec node->spec-k qualified-keyword?
  [node node?]
  (or (@*node-tag->spek-k (:tag node))
      :spacetools.spacedoc.node/known-node))


(defn-spec tag->spec-k qualified-keyword?
  [node-tag keyword?]
  (node->spec-k {:tag node-tag}))


(defn-spec all-tags (s/coll-of keyword? :kind set?)
  []
  (set (keys @*node-tag->spek-k)))


(defn-spec known-node? (s/nilable keyword?)
  [tag keyword?]
  ((all-tags) tag))


(s/def :spacetools.spacedoc.node/known-node
  (s/and node? #((all-tags) (:tag %))))


(defn-spec link->link-prefix string?
  [path string?]
  (->> (vals link-type->prefix)
       (filter (partial str/starts-with? path))
       (first)))


(def children-tag-s (comp (partial into #{} (map :tag)) :children))


(defn-spec fmt-problem string?
  [node node? problem map?]
  (str/join \newline
            (assoc problem
                   :node-tag (:tag node)
                   :spec-form (s/form (node->spec-k node)))))


(s/def ::problems (s/coll-of string? :min-count 1))

(defn-spec explain-deepest (s/nilable (s/keys :req [::problems]))
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  The function returns `nil` If all nodes are valid."
  [node node?]
  (or (when (nil? node) nil)
      (when-let [children (:children node)]
        (first (sequence (keep explain-deepest) children)))
      (when-not (s/valid? :spacetools.spacedoc.node/known-node node)
        (s/explain-data :spacetools.spacedoc.node/known-node node))
      (some->> node
               (s/explain-data (node->spec-k node))
               (:clojure.spec.alpha/problems)
               (r/map (partial fmt-problem node))
               (r/reduce str)
               (hash-map :problems))))


(defn-spec relation (s/map-of keyword? set?)
  "Return mapping between nodes and children sets."
  [parent node?]
  (r/reduce
   (r/monoid (fn [m n] (update m (:tag n) union (children-tag-s n))) hash-map)
   (tree-seq :children :children parent)))


(defn-spec relations (s/map-of keyword? set?)
  "Apply `relation` to PARENTS and `union` the outputs.."
  [parents (s/coll-of node?)]
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map relation parents)))


;;;; Table stuff

(defn-spec same-row-child-count? boolean?
  "Returns true if all rows have equal count of children.
:rule rows are ignored."
  ;; Note: Can't use row spec predicate here.
  [rows (s/coll-of (s/and node? #(= :table-row (:tag %))))]
  (let [t-c (remove #(#{:rule} (:type %)) rows)]
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


(defn-spec hl-val->gh-id-base (s/and string? #(re-matches #"#.+" %))
  [hl-value non-blank-string?]
  (str "#"
       (-> hl-value
           (str/replace " " "-")
           (str/lower-case)
           (str/replace #"[^\p{Nd}\p{L}\p{Pd}\p{Pc}]" ""))))


(defn-spec hl-val->path-id-frag non-blank-string?
  [hl-value non-blank-string?]
  (-> hl-value
      (str/lower-case)
      (str/replace #"[^\p{Nd}\p{L}\p{Pd}]" " ")
      (str/trim)
      (str/replace #"\s+" "_")))


(defn-spec path-id? boolean?
  [val any?]
  (and
   (string? val)
   (some?
    (re-matches
     ;; forgive me Father for I have sinned
     #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
     val))))


(defn-spec assoc-level-and-path-id node?
  "Fill node with :level and :path-id"
  ([node node?]
   (let [{tag :tag value :value} node]
     (assoc node
            :level 1
            :path-id (hl-val->path-id-frag value))))
  ([parent-node node? node node?]
   (let [{p-tag :tag p-level :level p-path-id :path-id} parent-node
         {tag :tag value :value} node
         hl-level (inc p-level)]
     (assoc node
            :level hl-level
            :path-id (str p-path-id "/" (hl-val->path-id-frag value))))))


(defn-spec root-node? boolean?
  [node any?]
  (s/valid? :spacetools.spacedoc.node/root node))


(defn-spec up-tags root-node?
  "Update #+TAGS `:spacetools.spacedoc.node/key-word` of the ROOT-NODE.
  SPACEROOT is the root directory of Spacemacs and FILE is the exported file
  name. they are used for creating basic tags if non is present."
  [spaceroot string? file string? root-node root-node?]
  root-node)

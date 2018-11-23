(ns spacetools.spacedoc.util
  "SDN manipulation utilities."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as conf]
            [spacetools.spacedoc.core :as sc :refer [node?]]))


(s/def ::spec-problem (s/keys :req [:clojure.spec.alpha/problems
                                    :clojure.spec.alpha/spec
                                    :clojure.spec.alpha/value]))


;;;; Generic stuff for SDN manipulation

(defn-spec non-blank-string? boolean?
  [s any?]
  (and (string? s)
       ((complement str/blank?) s)))


(defn-spec link->link-prefix string?
  [path string?]
  (->> (vals conf/link-type->prefix)
       (filter (partial str/starts-with? path))
       (first)))


(defn node->children-tag-s
  [node]
  (into #{} (map :tag) (:children node)))


(defn-spec fmt-problem string?
  [node node? problem map?]
  (str/join \newline
            (assoc problem
                   :node-tag (:tag node)
                   :spec-form (s/form (sc/node->spec-k node)))))


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
               (s/explain-data (sc/node->spec-k node))
               (:clojure.spec.alpha/problems)
               (r/map (partial fmt-problem node))
               (r/reduce str)
               (hash-map :problems))))


(defn-spec relation (s/map-of keyword? set?)
  "Return mapping between nodes and children sets."
  [parent node?]
  (r/reduce
   (r/monoid (fn [m n] (update m (:tag n) union (node->children-tag-s n)))
             hash-map)
   (tree-seq :children :children parent)))


(defn-spec relations (s/map-of keyword? set?)
  "Apply `relation` to PARENTS and `union` the outputs.."
  [parents (s/coll-of node?)]
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map relation parents)))


(defn-spec up-tags node?
  "Update #+TAGS `:spacetools.spacedoc.node/key-word` of the R-NODE.
R-NODE must be `:spacetools.spacedoc.node/root` node.
SPACEROOT is the root directory of Spacemacs.
SRC is the exported file name."
  [spaceroot string? src string? r-node node?]
  r-node)


;;;; Formatters

(defn-spec regex-pat? boolean?
  [obj any?]
  (= (type obj) java.util.regex.Pattern))


(def re-pats-union (memoize (fn [pats]
                              (->> pats
                                   (map #(str "(" % ")"))
                                   (interpose "|")
                                   (apply str)
                                   (re-pattern)))))


(defn-spec fmt-str string?
  [rep-map (s/map-of regex-pat? string?) text string?]
  ((fn [t]
     (let [ret (str/replace
                t
                (re-pats-union (keys rep-map))
                (fn [text-match]
                  (reduce
                   (fn [text-frag [pat rep]]
                     (if (re-matches pat text-frag)
                       (str/replace text-frag pat rep)
                       text-frag))
                   (first text-match)
                   rep-map)))]
       (if-not (= ret t)
         (recur ret)
         ret)))
   text))


(defn-spec fmt-text string?
  [text string?]
  (fmt-str conf/text-rep-map text))


(defn-spec fmt-link non-blank-string?
  [link-type keyword? link non-blank-string?]
  (if (= link-type :custom-id)
    (fmt-str conf/custom-id-link-rep-map link)
    link))


(defn-spec fmt-hl-val non-blank-string?
  [hl-val non-blank-string?]
  (if (= hl-val conf/toc-hl-val)
    conf/toc-hl-val
    (fmt-str conf/text-rep-map (str/trim hl-val))))


(defn-spec indent string?
  [indent-level nat-int? s string?]
  (if (str/blank? s)
    s
    (let [ind (apply str (repeat indent-level " "))
          trailing-ns (str/replace-first s (str/trim-newline s) "")
          lines (str/split-lines s)
          c-d (r/reduce (r/monoid
                         #(min %1 (- (count %2) (count (str/triml %2))))
                         (constantly (count s)))
                        (remove str/blank? lines))
          ws-prefix (apply str (repeat c-d " "))]
      (str
       (->> lines
            (r/map (comp #(if (str/blank? %) "\n" %)
                         #(str ind %)
                         #(str/replace-first % ws-prefix "")))
            (r/reduce
             (r/monoid
              #(str %1 (when (every? (complement str/blank?) [%1 %2]) "\n") %2)
              str)))
       (if (empty? trailing-ns)
         "\n"
         trailing-ns)))))


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

(defn-spec in-hl-level-range? boolean?
  [level nat-int?]
  (some? ((set (range 1 (inc conf/max-headline-depth))) level)))


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

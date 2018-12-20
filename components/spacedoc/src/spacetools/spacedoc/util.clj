(ns spacetools.spacedoc.util
  "Shared helpers name-space."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc :refer [node?]]))


(s/def ::spec-problem (s/keys :req [:clojure.spec.alpha/problems
                                    :clojure.spec.alpha/spec
                                    :clojure.spec.alpha/value]))


;;;; Generic stuff for SDN manipulation

(defn-spec non-blank-string? boolean?
  "Return true if X is a string and it contains non-blank characters."
  [x any?]
  (and (string? x)
       ((complement str/blank?) x)))


(defn-spec link->link-prefix string?
  "Given full link return corresponding prefix(usually protocol + ://)."
  [path string?]
  (->> (vals (cfg/link-type->prefix))
       (filter (partial str/starts-with? path))
       (first)))


(defn-spec node->children-tag-s keyword?
  "Return tags of the NODE direct children(non recursive)."
  [node node?]
  (into #{} (map :tag) (:children node)))


(defn-spec fmt-problem string?
  "Format `:clojure.spec.alpha/problems` into string."
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
  "Returns true if X is a regex pattern."
  [x any?]
  (instance? java.util.regex.Pattern x))


(def re-pats-union
  "Given regex pattern seq, return \"or\" union regexp pattern."
  (memoize (fn [re-pats]
             (->> re-pats
                  (map #(str "(" % ")"))
                  (interpose "|")
                  (apply str)
                  (re-pattern)))))


(defn-spec fmt-str string?
  "Format TEXT using REP-MAP(defaults to `cfg/text-rep-map`) replacement map."
  ([text string?]
   (fmt-str (cfg/text-rep-map) text))
  ([rep-map (s/map-of regex-pat? string?) text string?]
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
    text)))


(defn-spec fmt-link non-blank-string?
  "Format link value based on LINK-TYPE."
  [link-type keyword? link non-blank-string?]
  (if (= link-type :custom-id)
    (fmt-str (cfg/custom-id-link-rep-map) (str/lower-case link))
    link))


(defn-spec fmt-hl-val non-blank-string?
  "Format headline value string."
  [hl-val non-blank-string?]
  (if (= hl-val (cfg/toc-hl-val))
    (cfg/toc-hl-val)
    (fmt-str (cfg/text-rep-map) (str/trim hl-val))))


(defn-spec indent string?
  "Indent lines in string S with INDENT-LEVEL indentation."
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
  "Returns true if LEVEL is in [1..`cfg/max-headline-depth`] range."
  [level nat-int?]
  (some? ((set (range 1 (inc (cfg/max-headline-depth)))) level)))


(defn-spec hl-val->gh-id-base (s/and string? #(re-matches #"#.+" %))
  "Given HL-VALUE headline value return github style id base.
Base means that the value doesn't have -N post-fix used to resolve collisions."
  [hl-value non-blank-string?]
  (str "#"
       (-> hl-value
           (str/replace " " "-")
           (str/lower-case)
           (str/replace #"[^\p{Nd}\p{L}\p{Pd}\p{Pc}]" ""))))


(defn-spec hl-val->path-id-frag non-blank-string?
  "Given HL-VALUE headline value return path-id style id fragment.
Fragments are  particular headline values in the \"/\" separated chain."
  [hl-value non-blank-string?]
  (-> hl-value
      (str/lower-case)
      (str/replace #"[^\p{Nd}\p{L}\p{Pd}]" " ")
      (str/trim)
      (str/replace #"\s+" "_")))


(defn-spec path-id? boolean?
  "Return true if X is a path-id."
  [x any?]
  (and
   (string? x)
   (some?
    (re-matches
     ;; forgive me Father for I have sinned
     #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
     x))))


(defn-spec valid-node? boolean?
  "Return true if NODE is a valid node."
  [node any?]
  (s/valid? (sc/node->spec-k node) node))


(defn-spec hl? boolean?
  "Return true if NODE is a headline."
  [node any?]
  (= (:tag node) :headline))


(defn-spec hl->depth pos-int?
  "Return how deeply children of HL go.
Value of `:level` ignored."
  [hl hl?]
  ((fn rec [depth node]
     (if (hl? node)
       (inc (r/reduce (r/monoid max (constantly 0))
                      (r/map (partial rec depth) (:children node))))
       depth))
   0 hl))


(defn-spec valid-hl? boolean?
  "Return true if NODE is a valid headline."
  [node any?]
  (or (and (hl? node)
           (valid-node? node))))


;; TODO: Fix "over nesting" for headlines
(defn-spec assoc-level-and-path-id node? #_ valid-hl?
  "Fill node with :level and :path-id"
  ([node hl?]
   (let [{tag :tag value :value} node]
     (assoc node
            :level 1
            :path-id (hl-val->path-id-frag value))))
  ([parent-node hl? node hl?]
   (let [{tag :tag value :value} node
         hl-level (inc (:level parent-node))]
     (assoc node
            :level hl-level
            :path-id (str (:path-id parent-node)
                          "/" (hl-val->path-id-frag value))))))

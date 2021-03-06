(ns spacetools.spacedoc.util
  "Shared helpers name-space."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [medley.core :refer [update-existing]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc :refer [node?]]
            [spacetools.spacedoc.node :as n]))

;;;; Generic stuff for SDN manipulation

(defn-spec non-blank-string? boolean?
  "Return true if X is a string and it contains non-blank characters."
  [x any?]
  (and (string? x) ((complement str/blank?) x)))


(defn-spec node->children-tag-s (s/coll-of keyword? :kind set?)
  "Return tags of the NODE direct children(non recursive)."
  [node node?]
  (into #{} (map :tag) (:children node)))


(defn-spec fmt-problem string?
  "Formats one of `:clojure.spec.alpha/problems`."
  [node node? problem map?]
  (str/join \newline
            (assoc problem
                   :node-tag (:tag node)
                   :spec-form (s/form (sc/node->spec-k node)))))


(defn-spec remove-invalid (s/nilable sc/node?)
  "Removes all invalid SDN nodes.
NOTE: Returns nil if the top node is or becomes invalid after
children removal."
  [maybe-node any?]
  (as-> maybe-node n
    (update-existing n :children (partial into [] (comp (map remove-invalid)
                                                        (remove nil?))))
    (when (s/valid? ::n/any-node n) n)))


(s/def ::maybe-problems (s/nilable (s/keys :req [::s/problems])))

(defn-spec explain-deepest ::maybe-problems
  "Validates each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  If multiply children of the same node are invalid the first one
  will be reported.
  The function returns `nil` If all nodes are valid."
  [node any?]
  (or (first (sequence (keep explain-deepest) (:children node)))
      (s/explain-data ::n/any-node node)))


(defn-spec relation (s/map-of keyword? set?)
  "Returns mapping between nodes and children sets."
  [parent node?]
  (r/reduce
   (r/monoid (fn [m n] (update m (:tag n)
                              (fnil union #{} #{})
                              (node->children-tag-s n)))
             hash-map)
   (tree-seq :children :children parent)))


(s/def ::key->set (s/map-of keyword? set?))
(s/def ::nodes (s/coll-of node?))

(defn-spec relations ::key->set
  "Apply `relation` to PARENTS and `union` the outputs.."
  [parents ::nodes]
  (r/fold (r/monoid (partial merge-with union) hash-map)
          (r/map relation parents)))


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
   (if (empty? rep-map)
     text
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
      text))))


(defn-spec fmt-link non-blank-string?
  "Format link value based on LINK-TYPE."
  ([link-type keyword? link non-blank-string?]
   (fmt-link (cfg/custom-id-link-rep-map) link-type link))
  ([rep-map (s/map-of regex-pat? string?)
    link-type keyword?
    link non-blank-string?]
   (if (= link-type :custom-id)
     (fmt-str rep-map (str/lower-case link))
     link)))


(defn-spec fmt-hl-val non-blank-string?
  "Format headline value string."
  ([rep-map (s/map-of regex-pat? string?) hl-val non-blank-string?]
   (if (= hl-val (cfg/toc-hl-val))
     (cfg/toc-hl-val)
     (fmt-str rep-map (str/trim hl-val))))
  ([hl-val non-blank-string?]
   (fmt-hl-val (cfg/text-rep-map) (str/trim hl-val))))


(defn-spec indent string?
  "Indent lines in string S with INDENT-LEVEL indentation."
  [indent-level nat-int? s string?]
  (if (str/blank? s)
    s
    (let [ind (str/join (repeat indent-level " "))
          trailing-ns (str/replace-first s (str/trim-newline s) "")
          lines (str/split-lines s)
          c-d (r/reduce (r/monoid
                         #(min %1 (- (count %2) (count (str/triml %2))))
                         (constantly (count s)))
                        (remove str/blank? lines))
          ws-prefix (str/join (repeat c-d " "))]
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


(defn-spec valid-node? boolean?
  "Return true if NODE is a valid node."
  [node any?]
  (and (some? ((sc/all-tags) (:tag node)))
       (s/valid? (sc/node->spec-k node) node)))


;;;; Headline stuff

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


(defn-spec hl? boolean?
  "Return true if NODE is a headline."
  [node any?]
  (= (:tag node) :headline))


(defn-spec valid-hl? boolean?
  "Return true if NODE is a valid headline."
  [node any?]
  (and (hl? node) (valid-node? node)))


(defn-spec valid-root? boolean?
  "Return true if NODE is a valid root node."
  [node any?]
  (s/valid? ::n/root node))


(defn-spec flatten-headline valid-hl?
  "Flatten HL headline children by converting them into sections"
  [level (s/and pos-int? #(<= % (cfg/max-headline-depth))) hl hl?]
  ((fn rec [depth idx {:keys [value children todo?] :as node}]
     (if (hl? node)
       (let [children (vec (map-indexed (partial rec (inc depth)) children))]
         (if (>= depth level)
           (->> children
                (r/mapcat
                 #(if (= :section (:tag %))
                    (:children %)
                    [%]))
                (into [(n/paragraph
                        (n/text (str
                                 (if todo? "TODO: " "")
                                 (when-not (zero? idx) "\n")
                                 value \newline)))])
                (apply n/section))
           (assoc node :children children)))
       node))
   0 0 hl))

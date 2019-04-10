(ns spacetools.spacedoc.org
  "Exporting to .org format."
  (:require [clojure.core.match :refer [match]]
            [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [clojure.string :as str :refer [join]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu :refer [hl? valid-hl?]]))


(def block-container-delims
  "Delimiters surrounding block elements."
  {:verse ["#+BEGIN_VERSE\n"
           "#+END_VERSE\n"]
   :quoted ["#+BEGIN_QUOTE\n"
            "#+END_QUOTE\n"]
   :section ["" ""]})

(def indirect-nodes
  "These nodes can be converted only in their parent context."
  #{:item-children :item-tag :table-row :table-cell})

(def kinds
  "Node families. Used mainly to figure out how to split nodes."
  {(sc/inline-container-tags) :inline-container
   (sc/inline-leaf-tags) :inline-leaf
   (sc/block-tags) :block})


(defmulti sdn->org
  "Given node return its org-mode text representation."
  (fn [{tag :tag :as node}]
    {:pre  [((complement indirect-nodes) tag)
            (map? node)
            (keyword? tag)]}
    (cond
      ;; List node group.
      (#{:feature-list :plain-list} tag) :list

      ;; Emphasis containers.
      (#{:bold :italic :underline :strike-through} tag) :emphasis-container

      ;; Block-container node group.
      ((set (keys block-container-delims)) tag) :block-container

      ;; Everything else.
      :else tag)))


;;;;  TOC spec and constructor

;; TOC leaf (GitHub style link to a local headline)

(s/def :spacetools.spacedoc.org.toc.leaf/tag #{:link})
(s/def :spacetools.spacedoc.org.toc.leaf/type #{:custom-id})
(s/def :spacetools.spacedoc.org.toc.leaf/path
  :spacetools.spacedoc.node.link/path)
(s/def :spacetools.spacedoc.org.toc.leaf/children
  (s/coll-of :spacetools.spacedoc.node/text
             :kind vector?
             :min-count 1
             :into []))
(s/def ::toc-leaf (s/keys :req-un
                          [:spacetools.spacedoc.org.toc.leaf/tag
                           :spacetools.spacedoc.org.toc.leaf/type
                           :spacetools.spacedoc.org.toc.leaf/path
                           :spacetools.spacedoc.org.toc.leaf/children]))

;; TOC item children wrapper (item-children)

(s/def :spacetools.spacedoc.org.toc.item-children/tag #{:item-children})
(s/def :spacetools.spacedoc.org.item-children/children
  (s/cat
   :headline-link ::toc-leaf
   :sub-headlines-links (s/?
                         (s/cat :line-break :spacetools.spacedoc.node/line-break
                                :brench (s/+ ::toc-branch)))))
(s/def ::toc-item-children
  (s/keys :req-un [:spacetools.spacedoc.org.toc.item-children/tag
                   :spacetools.spacedoc.org.item-children/children]))

;; TOC item (unordered list item)

(s/def :spacetools.spacedoc.org.toc.item/tag #{:list-item})
(s/def :spacetools.spacedoc.org.toc.item/type #{:unordered})
(s/def :spacetools.spacedoc.org.toc.item/bullet #{"- "})
(s/def :spacetools.spacedoc.org.toc.item/checkbox nil?)
(s/def :spacetools.spacedoc.org.toc.item/children (s/coll-of ::toc-item-children
                                                             :kind vector?
                                                             :min-count 1
                                                             :into []))
(s/def ::toc-item (s/keys :req-un
                          [:spacetools.spacedoc.org.toc.item/tag
                           :spacetools.spacedoc.org.toc.item/type
                           :spacetools.spacedoc.org.toc.item/bullet
                           :spacetools.spacedoc.org.toc.item/checkbox
                           :spacetools.spacedoc.org.toc.item/children]))


;; TOC branch (unordered list)

(s/def :spacetools.spacedoc.org.toc.branch/tag #{:plain-list})
(s/def :spacetools.spacedoc.org.toc.branch/type #{:unordered})
(s/def :spacetools.spacedoc.org.toc.branch/children (s/coll-of ::toc-item
                                                               :kind vector?
                                                               :min-count 1
                                                               :into []))
(s/def ::toc-branch
  (s/keys :req-un [:spacetools.spacedoc.org.toc.branch/tag
                   :spacetools.spacedoc.org.toc.branch/type
                   :spacetools.spacedoc.org.toc.branch/children]))


;; TOC items wrapper (section)

(s/def :spacetools.spacedoc.org.toc.wrapper/tag #{:section})
(s/def :spacetools.spacedoc.org.toc.wrapper/children (s/coll-of ::toc-branch
                                                                :kind vector?
                                                                :min-count 1
                                                                :into []))
(s/def ::toc-wrapper (s/keys :req-un
                             [:spacetools.spacedoc.org.toc.wrapper/tag
                              :spacetools.spacedoc.org.toc.wrapper/children]))

;; TOC (headline)

(s/def :spacetools.spacedoc.org.toc/tag #{:headline})
(s/def :spacetools.spacedoc.org.toc/todo? false?)
(s/def :spacetools.spacedoc.org.toc/children
  (s/coll-of ::toc-wrapper
             :kind vector?
             :max-count 1
             :into []))
(s/def :spacetools.spacedoc.org.toc/value
  #(= (cfg/toc-hl-val) %))
(s/def ::toc (s/keys :req-un [:spacetools.spacedoc.org.toc/tag
                              :spacetools.spacedoc.org.toc/todo?
                              :spacetools.spacedoc.org.toc/value
                              :spacetools.spacedoc.org.toc/children]))


(defn-spec gen-toc (s/nilable ::toc)
  "Generate table of content for ROOT node.
Return nil if ROOT node doesn't have any headlines."
  [{children :children :as root} :spacetools.spacedoc.node/root]
  (letfn [(hl->gid-base [headlin] (sdu/hl-val->gh-id-base
                                   (sdu/fmt-hl-val (:value headlin))))

          (gh-id [gid-bs cnt] (if (> cnt 1) (str gid-bs "-" (dec cnt)) gid-bs))

          (up-*gid->count! [*gc hl]
            (let [gid-base (hl->gid-base hl)]
              ((vswap! *gc update gid-base (fnil inc 0)) gid-base)))

          (hl->toc-el [{:keys [toc-wrapper? value gh-id children]}]
            (if toc-wrapper?
              (some->> children
                       (apply n/section)
                       (n/headline (cfg/toc-hl-val)))
              (n/unordered-list
               (vec (list* (n/link gh-id (n/text (sdu/fmt-hl-val value)))
                           (when (seq children)
                             (list* (n/line-break) children)))))))

          (hls->toc [headlines]
            ;; NOTE: We use local `volatile!` because with the counter state
            ;;       threading the code gates much more complex.
            (let [*gid->count (volatile! {})]
              ((fn inner [depth hl]
                 (-> hl
                     (assoc :gh-id
                            (when-not (:toc-wrapper? hl)
                              (gh-id
                               (hl->gid-base hl)
                               (up-*gid->count! *gid->count hl))))
                     (update :children
                             #(when (< depth (cfg/toc-max-depth))
                                (when-let [hls (seq (filter hl? %))]
                                  (mapv (partial inner (inc depth)) hls))))
                     hl->toc-el))
               0 {:toc-wrapper? true :children (vec headlines)})))]

    (->> children
         (filter hl?)
         (hls->toc))))


;;;; full-root node spec

(s/def :spacetools.spacedoc.org.title-wrapper/children
  (s/cat :title :spacetools.spacedoc.node.meta/title))

(s/def ::title-wrapper
  (s/keys :req-un [:spacetools.spacedoc.node.section/tag
                   :spacetools.spacedoc.org.title-wrappr/children]))

(s/def :spacetools.spacedoc.org.tags-wrapper/children
  (s/cat :tags :spacetools.spacedoc.node.meta/tags))

(s/def ::tags-wrapper
  (s/keys :req-un [:spacetools.spacedoc.node.section/tag
                   :spacetools.spacedoc.org.tags-wrapper/children]))

(s/def :spacetools.spacedoc.org.full-root/children
  (s/cat
   :title ::title-wrapper
   :tags ::tags-wrapper
   :logo (s/* :spacetools.spacedoc.node/section)
   :toc (s/? ::toc)
   :rest (s/* :spacetools.spacedoc.node/root-child)))

(s/def ::full-root
  (s/merge :spacetools.spacedoc.node/root
           (s/keys :req-un [:spacetools.spacedoc.org.full-root/children])))


;;;; root node helpers

(defn-spec assoc-toc :spacetools.spacedoc.node/root
  "Add Table of content based on headlines present in the ROOT node"
  [{children :children :as root} :spacetools.spacedoc.node/root]
  (if-let [toc (gen-toc root)]
    (let [[b-toc a-toc] (split-with (complement hl?) children)]
      (assoc root :children (vec (concat b-toc [toc] a-toc))))
    root))


(defn-spec assoc-title :spacetools.spacedoc.node/root
  "Add title node"
  [title (s/and string? (complement str/blank?))
   {children :children :as root} :spacetools.spacedoc.node/root]
  (update root :children
          (partial apply vector (n/section (n/key-word "TITLE" title)))))


(defn-spec assoc-tags :spacetools.spacedoc.node/root
  "Add tags node"
  [tags (s/coll-of (s/and string? (complement str/blank?))
                   :min-count 1)
   {children :children :as root} :spacetools.spacedoc.node/root]
  (update root :children
          (partial apply vector (->> tags
                                     (join "|")
                                     (n/key-word "TAGS")
                                     n/section))))


(s/def ::special-node (s/or :title ::title-wrapper
                            :tags ::tags-wrapper
                            :toc ::toc))

(defn-spec normalize-root-children ::full-root
  "Assoc Title tags and TOC nodes to the ROOT node children.
NOTE: If ROOT doesn't have title and tags values placeholders will be used.
NOTE: Existing nodes will be replaced."
  [root :spacetools.spacedoc.node/root]
  (->> (update root :children (partial remove #(s/valid? ::special-node %)))
       assoc-toc
       (assoc-tags (:tags root ["Untagged"]))
       (assoc-title (:title root "Untitled"))))


;;;; Helpers

(defn-spec assoc-level-and-path-id valid-hl?
  "Fill node with :level and :path-id"
  ([node hl?]
   (let [{tag :tag value :value} node]
     (assoc node
            :level 1
            :path-id (sdu/hl-val->path-id-frag value))))
  ([parent-node hl? node hl?]
   (let [{tag :tag value :value} node
         hl-level (inc (:level parent-node))]
     (assoc node
            :level hl-level
            :path-id (str (:path-id parent-node)
                          "/" (sdu/hl-val->path-id-frag value))))))


(defn-spec viz-len nat-int?
  "Like `count` but returns real visual length of a string."
  [^String s string?]
  (-> s
      (str/replace #"\[(?:\[[^\[\]]+\]){0,1}\[([^\[\]]+)\]+\]" "$1")
      (str/replace "\u200B" "")
      (count)))


(defn-spec tag->kind (s/nilable keyword?)
  "Given node tag return its family. See `kinds`."
  [tag (s/nilable keyword?)]
  (some->> kinds
           (filter #((key %) tag))
           (first)
           (val)))


(defn-spec sep-inlines (s/nilable string?)
  "Separate inline elements."
  [t1 (s/nilable keyword?) s1 (s/nilable string?) t2 keyword? s2 string?]
  (when (and (every? not-empty [s1 s2])
             (not=  :text t1 t2))
    (let [l-s1-sep? ((cfg/seps-right) (last s1))
          f-s2-sep? ((cfg/seps-left) (first s2))]
      (when-not (or l-s1-sep? f-s2-sep?) " "))))


(defn-spec sep-blocks (s/nilable string?)
  "Separate block elements."
  [p-t keyword? t1 (s/nilable keyword?) t2 keyword?]
  (let [[t1-k t2-k] (mapv tag->kind [t1 t2])]
    (match [p-t            t1          t1-k        t2            t2-k     ]
           [:item-children :paragraph  _           ::end         _        ] ""
           [:item-children :plain-list _           ::end         _        ] ""
           [:item-children _           :block      ::end         _        ] "\n"
           [_              _           _           ::end         _        ] ""
           [:section       nil         _           :table        _        ] "\n"
           [_              nil         _           _             _        ] ""
           [_              _           _           :plain-list   _        ] ""
           [_              _           _           :feature-list _        ] ""
           [_              :headline   _           _             _        ] "\n"
           [_              _           _           :headline     _        ] "\n"
           [_              _           :block      _             _        ] "\n"
           [_              _           _           _             :block   ] "\n"
           :else nil)))


(defn-spec conv* (s/coll-of string? :kind vector?)
  "Reduce CHILDREN vector into vector of ORG string representations inserting
  proper separators (new lies and white spaces).
  P-TAG is parent node tag."
  [p-tag keyword? children (s/coll-of sc/node? :kind vector?)]
  (r/reduce (r/monoid
             (fn [acc [n-t n-s]]
               (let [{h-t :head-tag h :head} (meta acc)
                     next (str (or (sep-inlines h-t h n-t n-s)
                                   (sep-blocks p-tag h-t n-t)
                                   "")
                               n-s)]
                 (with-meta (conj acc next) {:head next :head-tag n-t})))
             vector)
            (r/map #(vector (:tag %) (sdn->org %))
                   (conj children {:tag ::end}))))


(defn-spec conv string?
  "Reduce CHILDREN vector into `str/join`ed ORG string using `conv*`
  P-TAG is parent node tag."
  [p-tag keyword? children (s/coll-of sc/node? :kind vector?)]
  (join (conv* p-tag children)))


;;;; Groups of nodes (many to one).

(defmethod sdn->org :emphasis-container
  [{:keys [tag children]}]
  (let [token ((cfg/emphasis-tokens) tag)]
    (str token (conv tag children) token)))


(defmethod sdn->org :list
  [{:keys [tag children]}]
  (str (str/trim-newline (conv tag children)) "\n"))


(defmethod sdn->org :block-container
  [{:keys [tag children]}]
  (let [{[begin-token end-token] tag} block-container-delims]
    (str begin-token
         ;; NOTE: We don't "hard-code" indentation into sections
         (sdu/indent (if (= tag :section) 0 (cfg/begin-end-indentation))
                     (conv tag children))
         end-token)))


;;;; Individual nodes (one to one).

(defmethod sdn->org :paragraph
  [{:keys [tag children]}]
  (conv tag children))


(defn-spec table->vec-rep (s/cat :cols-width (s/nilable (s/coll-of nat-int?))
                                 :cols (s/+ (s/nilable (s/coll-of string?))))
  "Return vector representation of table ROWS."
  [{rows :children} :spacetools.spacedoc.node/table]
  (let [vec-tab (mapv
                 (fn [{type :type cells :children}]
                   (when (= type :standard)
                     (mapv #(str " " (conv :table-row (:children %)) " ")
                           cells)))
                 rows)
        cols-w (when-let [ne-vec-tab (seq (remove empty? vec-tab))]
                 (apply mapv
                        (fn [& cols]
                          (apply max (map viz-len cols)))
                        ne-vec-tab))]
    (vec (concat [cols-w] vec-tab))))


(defn-spec table-ruler string?
  "Generate WIDTH wide table ruler."
  [cell-width (s/coll-of nat-int?)]
  (join "+" (map #(join (repeat % "-")) cell-width)))


(defn-spec table-row string?
  "Join table cells of ROW into string."
  [row (s/coll-of string?)
   cell-width (s/coll-of pos-int?)]
  (join "|"
        (map (fn [column-width cell]
               (str cell (join (repeat (- column-width
                                          (viz-len cell))
                                       " "))))
             cell-width
             row)))


(defmethod sdn->org :table
  [table]
  (str/replace-first
   (let [[cols-w & vrep] (table->vec-rep table)]
     (sdu/indent (cfg/table-indentation)
                 (r/fold (r/monoid #(join "\n" [%1 %2]) str)
                         (r/map (comp (partial format "|%s|")
                                      #(cond (empty? cols-w) "" ;; <- no cols
                                             ;; Empty cols are rulers.
                                             (empty? %) (table-ruler cols-w)
                                             :else (table-row % cols-w)))
                                vrep))))
   #"^\n" ""))


(defmethod sdn->org :link
  [{:keys [tag type path children]}]
  (format "[[%s]%s]"
          (sdu/fmt-link type path)
          (if (seq children)
            (format "[%s]" (str/trim (conv tag children)))
            "")))


(defmethod sdn->org :list-item
  [{b :bullet c :checkbox [{:keys [tag children]} item-tag] :children}]
  (let [itag (some->> item-tag
                      (:children)
                      (conv :item-tag))
        last-child-tag (->> children last :tag)
        last-child-kind (tag->kind last-child-tag)
        pref (str (str/trim b)
                  " "
                  (when itag (format "%s :: " itag)))]
    (apply str
           pref
           (->> children
                (conv tag)
                (sdu/indent (count pref))
                (str/triml)))))


(defmethod sdn->org :example
  [{value :value}]
  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n"
          (sdu/indent (cfg/begin-end-indentation) value)))


(defmethod sdn->org :src
  [{:keys [language value]}]
  (format "#+BEGIN_SRC %s\n%s#+END_SRC\n"
          language
          (sdu/indent (cfg/begin-end-indentation) value)))


(defmethod sdn->org :text
  [{value :value}]
  (sdu/fmt-str value))


(defmethod sdn->org :line-break
  [_]
  "\n")


(defmethod sdn->org :key-word
  [{:keys [key value]}]
  (format "#+%s: %s\n" key value))


(defmethod sdn->org :headline
  [{:keys [value children todo?] :as hl}]
  (let [f-val (sdu/fmt-hl-val value)
        headline (-> hl
                     (update :path-id #(or % (sdu/hl-val->path-id-frag f-val)))
                     (assoc :value f-val)
                     (update :level #(or % 1)))]
    (str (join (repeat (:level headline) "*"))
         " "
         (when todo? "TODO ")
         f-val
         "\n"
         (conv :headline
               (mapv #(if (sdu/hl? %) (assoc-level-and-path-id headline %) %)
                     children)))))


(defmethod sdn->org :verbatim
  [{:keys [tag value]}]
  (let [token ((cfg/emphasis-tokens) tag)]
    (str token value token)))


(defmethod sdn->org :kbd
  [{:keys [tag value]}]
  (let [token ((cfg/emphasis-tokens) tag)]
    (str token (join " " value) token)))


(defmethod sdn->org :root
  [{:keys [tag] :as root}]
  (->> root
       (normalize-root-children)
       (:children)
       (mapv #(if (sdu/hl? %) (assoc-level-and-path-id %) %))
       (conv tag)))


;;;; Pseudo-nodes

(defmethod sdn->org ::end [_] "")

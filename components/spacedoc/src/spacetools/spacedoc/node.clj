(ns spacetools.spacedoc.node
  "Node specs and constructors. All public functions are constructors."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node-impl :refer [defnode defnode*]]))


;;;; Value specs

;; NOTE: Some lines may be empty but not all of them.
(s/def :spacetools.spacedoc.node.val/non-blank-lines
  (s/with-gen (s/and string? (complement str/blank?))
    #(gen/fmap str
               (gen/tuple
                (gen/string-alphanumeric)
                (gen/elements ["\n" ""])
                (gen/string-alphanumeric
                 (gen/elements ["\n" ""])
                 (gen/string-alphanumeric))))))


(s/def :spacetools.spacedoc.node.val/non-blank-string
  (s/and string? (complement str/blank?)))


(s/def :spacetools.spacedoc.node.val/path-id
  (s/with-gen
    (s/and
     string?
     #(re-matches
       ;; forgive me Father for I have sinned
       #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
       %))
    #(gen/fmap
      (fn [[a delm b]] (str/lower-case (str a delm b)))
      (gen/tuple
       (gen/string-alphanumeric)
       (gen/elements ["/" ""])
       (gen/string-alphanumeric)))))


;;;; Node specs


;; Any node should satisfy this spec

(s/def :spacetools.spacedoc.node.any-node/tag keyword?)
(s/def :spacetools.spacedoc.node.any-node/children (s/coll-of
                                                    ::any-node
                                                    :min-count 0
                                                    :kind vector?
                                                    :into []))
(s/def ::any-node (s/keys
                   :req-un [:spacetools.spacedoc.node.any-node/tag]
                   :opt-un [:spacetools.spacedoc.node.any-node/children]))


;; inline leaf

(defmethod sc/inline-leaf :kbd [_] ::kbd)
(defmethod sc/inline-leaf :line-break [_] ::line-break)
(defmethod sc/inline-leaf :text [_] ::text)
(defmethod sc/inline-leaf :verbatim [_] ::verbatim)
(s/def ::inline-leaf (s/multi-spec sc/inline-leaf :tag))


;; kbd node

(s/def :spacetools.spacedoc.node.kbd/value
  (s/with-gen (s/coll-of :spacetools.spacedoc.node.val/non-blank-string
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen :spacetools.spacedoc.node.val/non-blank-string) 1 3)))


(defnode ::kbd (s/keys :req-un [:spacetools.spacedoc.node.kbd/value]))


;; line-break node

(defnode ::line-break (s/keys :req-un []))


;; text node

(s/def :spacetools.spacedoc.node.text/value string?)
(defnode ::text (s/keys :req-un [:spacetools.spacedoc.node.text/value]))


;; verbatim node

(s/def :spacetools.spacedoc.node.verbatim/value
  :spacetools.spacedoc.node.val/non-blank-lines)
(defnode ::verbatim (s/keys :req-un [:spacetools.spacedoc.node.verbatim/value]))


;; inline element

(s/def ::inline-element (s/or :inline-container ::inline-container
                              :inline-leaf ::inline-leaf))


;; inline-container children

(s/def ::inline-container-children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::inline-element) 1 3)))


;; bold node

(s/def :spacetools.spacedoc.node.bold/children ::inline-container-children)
(defnode ::bold (s/keys :req-un [:spacetools.spacedoc.node.bold/children]))


;; italic node

(s/def :spacetools.spacedoc.node.italic/children
  ::inline-container-children)
(defnode ::italic (s/keys :req-un [:spacetools.spacedoc.node.italic/children]))


;; strike-through node

(s/def :spacetools.spacedoc.node.strike-through/children
  ::inline-container-children)
(defnode ::strike-through
  (s/keys :req-un [:spacetools.spacedoc.node.strike-through/children]))


;; subscript node

(s/def :spacetools.spacedoc.node.subscript/children
  ::inline-container-children)
(defnode ::subscript
  (s/keys :req-un [:spacetools.spacedoc.node.subscript/children]))


;; superscript node

(s/def :spacetools.spacedoc.node.superscript/children
  ::inline-container-children)
(defnode ::superscript
  (s/keys :req-un [:spacetools.spacedoc.node.superscript/children]))


;; underline node

(s/def :spacetools.spacedoc.node.underline/children
  ::inline-container-children)
(defnode ::underline
  (s/keys :req-un [:spacetools.spacedoc.node.underline/children]))


;; link

(s/def :spacetools.spacedoc.node.link/type (s/with-gen #((cfg/link-types) %)
                                             #(gen/elements (cfg/link-types))))
(s/def :spacetools.spacedoc.node.link/path
  (s/with-gen (s/and string?
                     #(re-matches
                       (re-pattern
                        (str "^(?:"
                             (str/join "|"
                                       (map str/re-quote-replacement
                                            (vals (cfg/link-type->prefix))))
                             ").+$"))
                       %))
    #(gen/fmap (fn [[prefix path]] (str prefix path))
               (gen/tuple
                (gen/elements (vals (cfg/link-type->prefix)))
                (gen/not-empty (gen/string-alphanumeric))))))
(s/def :spacetools.spacedoc.node.link/children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 0
                         :into [])
    #(gen/vector (s/gen ::inline-element) 0 3)))
(defnode* ::link (s/keys :req-un
                         [:spacetools.spacedoc.node.link/type
                          :spacetools.spacedoc.node.link/path
                          :spacetools.spacedoc.node.link/children]))


;; paragraph node

(s/def ::paragraph-child ::inline-element)
(s/def :spacetools.spacedoc.node.paragraph/children
  (s/with-gen (s/coll-of ::paragraph-child
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::paragraph-child) 1 3)))
(defnode ::paragraph
  (s/keys :req-un [:spacetools.spacedoc.node.paragraph/children]))


;; inline container

(defmethod sc/inline-container :bold [_] ::bold)
(defmethod sc/inline-container :italic [_] ::italic)
(defmethod sc/inline-container :link [_] ::link)
(defmethod sc/inline-container :strike-through [_] ::strike-through)
(defmethod sc/inline-container :subscript [_] ::subscript)
(defmethod sc/inline-container :superscript [_] ::superscript)
(defmethod sc/inline-container :underline [_] ::underline)
(s/def ::inline-container (s/multi-spec sc/inline-container :tag))


;; center node

(s/def ::center-child ::inline-element)
(s/def :spacetools.spacedoc.node.center/children
  (s/with-gen (s/coll-of ::center-child
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::center-child) 1 3)))
(defnode ::center (s/keys :req-un [:spacetools.spacedoc.node.center/children]))


;; example node

(s/def :spacetools.spacedoc.node.example/value
  :spacetools.spacedoc.node.val/non-blank-lines)
(defnode ::example (s/keys :req-un [:spacetools.spacedoc.node.example/value]))


;; item-children

(s/def :spacetools.spacedoc.node.item-children/child
  (s/or :block-element ::block-element
        :inline-element ::inline-element))
(s/def :spacetools.spacedoc.node.item-children/children
  (s/with-gen (s/coll-of :spacetools.spacedoc.node.item-children/child
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen :spacetools.spacedoc.node.item-children/child)
                 1 3)))
(defnode ::item-children
  (s/keys :req-un [:spacetools.spacedoc.node.item-children/children]))


;; item-tag

(s/def :spacetools.spacedoc.node.item-tag/child ::inline-element)
(s/def :spacetools.spacedoc.node.item-tag/children
  (s/with-gen (s/coll-of :spacetools.spacedoc.node.item-tag/child
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen :spacetools.spacedoc.node.item-tag/child) 1 3)))
(defnode ::item-tag (s/keys :req-un
                            [:spacetools.spacedoc.node.item-tag/children]))


;; list-item

(s/def :spacetools.spacedoc.node.list-item/type
  #{:ordered :unordered :descriptive})
(s/def :spacetools.spacedoc.node.list-item/bullet
  :spacetools.spacedoc.node.val/non-blank-string)
(s/def :spacetools.spacedoc.node.list-item/checkbox
  (s/nilable #{:trans :off :on}))
(s/def :spacetools.spacedoc.node.list-item*/children-list
  (s/cat :children ::item-children
         :tag (s/? ::item-tag)))
(s/def :spacetools.spacedoc.node.list-item/children
  (s/with-gen :spacetools.spacedoc.node.list-item*/children-list
    #(gen/fmap vec (s/gen :spacetools.spacedoc.node.list-item*/children-list))))
(defnode ::list-item (s/keys :req-un
                             [:spacetools.spacedoc.node.list-item/type
                              :spacetools.spacedoc.node.list-item/bullet
                              :spacetools.spacedoc.node.list-item/checkbox
                              :spacetools.spacedoc.node.list-item/children]))


;; feature-list node

(s/def :spacetools.spacedoc.node.feature-list/type
  #{:ordered :unordered :descriptive})
(s/def :spacetools.spacedoc.node.feature-list/children
  (s/with-gen (s/coll-of ::list-item
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::list-item) 1 3)))
(defnode* ::feature-list
  (s/keys :req-un [:spacetools.spacedoc.node.feature-list/type
                   :spacetools.spacedoc.node.feature-list/children]))


;; plain-list node

(s/def :spacetools.spacedoc.node.plain-list/type
  #{:ordered :unordered :descriptive})
(s/def :spacetools.spacedoc.node.plain-list/children
  (s/with-gen (s/coll-of ::list-item
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::list-item) 1 3)))
(defnode* ::plain-list
  (s/keys :req-un [:spacetools.spacedoc.node.plain-list/type
                   :spacetools.spacedoc.node.plain-list/children]))


;; quote node

(s/def :spacetools.spacedoc.node.quote/children
  (s/with-gen (s/coll-of ::paragraph
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::paragraph) 1 3)))
(defnode ::quote (s/keys :req-un [:spacetools.spacedoc.node.quote/children]))


;; src node

(s/def :spacetools.spacedoc.node.src/language
  :spacetools.spacedoc.node.val/non-blank-string)
(s/def :spacetools.spacedoc.node.src/value
  :spacetools.spacedoc.node.val/non-blank-lines)
(defnode ::src (s/keys :req-un [:spacetools.spacedoc.node.src/language
                                :spacetools.spacedoc.node.src/value]))


;; table-cell node

(s/def :spacetools.spacedoc.node.table-cell/children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 0
                         :into [])
    #(gen/vector (s/gen ::inline-element) 0 3)))
(defnode ::table-cell
  (s/keys :req-un [:spacetools.spacedoc.node.table-cell/children]))


;; table-row node

(s/def :spacetools.spacedoc.node.table-row/type #{:rule :standard})
(s/def :spacetools.spacedoc.node.table-row/children
  (s/with-gen (s/coll-of ::table-cell
                         :kind vector?
                         :min-count 0
                         :into [])
    #(gen/vector (s/gen ::table-cell) 0 3)))
(defnode ::table-row
  (s/keys :req-un [:spacetools.spacedoc.node.table-row/type
                   :spacetools.spacedoc.node.table-row/children]))


;; table node

(s/def :spacetools.spacedoc.node.table/type #{:org})
(s/def :spacetools.spacedoc.node.table/children
  (s/with-gen (s/and (s/coll-of ::table-row
                                :kind vector?
                                :min-count 1
                                :into [])
                     (fn same-row-child-count?
                       [rows]
                       (let [t-c (remove #(#{:rule} (:type %)) rows)]
                         (or (empty? t-c)
                             (apply = (map #(count (:children %)) t-c))))))
    #(gen/fmap (fn [[cells cells-per-row]]
                 (mapv
                  (fn [row-children]
                    {:tag :table-row
                     :type :standard
                     :children (vec row-children)})
                  (partition
                   cells-per-row
                   cells-per-row
                   ;; FIXME Split out `:rule` rows into separate thing.
                   (repeatedly (constantly {:tag :table-cell :children []}))
                   cells)))
               (gen/tuple
                (gen/vector
                 (gen/one-of
                  [(gen/return {:tag :table-cell
                                :type :rule
                                :children []})
                   (s/gen ::table-cell)])
                 1
                 6)
                (gen/elements (range 1 3))))))
(defnode* ::table
  (s/keys :req-un [:spacetools.spacedoc.node.table/type
                   :spacetools.spacedoc.node.table/children]))


;; verse node

(s/def :spacetools.spacedoc.node.verse/children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::inline-element) 1 3)))
(defnode ::verse (s/keys :req-un [:spacetools.spacedoc.node.verse/children]))


;; key-word node

(s/def :spacetools.spacedoc.node.key-word/key
  :spacetools.spacedoc.node.val/non-blank-string)
(s/def :spacetools.spacedoc.node.key-word/value
  :spacetools.spacedoc.node.val/non-blank-string)
(defnode ::key-word (s/keys :req-un [:spacetools.spacedoc.node.key-word/key
                                     :spacetools.spacedoc.node.key-word/value]))


;; block group

(defmethod sc/block-element :center [_] ::center)
(defmethod sc/block-element :example [_] ::example)
(defmethod sc/block-element :paragraph [_] ::paragraph)
(defmethod sc/block-element :feature-list [_] ::feature-list)
(defmethod sc/block-element :plain-list [_] ::plain-list)
(defmethod sc/block-element :quote [_] ::quote)
(defmethod sc/block-element :src [_] ::src)
(defmethod sc/block-element :table [_] ::table)
(defmethod sc/block-element :verse [_] ::verse)
(defmethod sc/block-element :key-word [_] ::key-word)
(s/def ::block-element (s/multi-spec sc/block-element :tag))


;; section node

(s/def :spacetools.spacedoc.node.section/children
  (s/with-gen (s/coll-of ::block-element
                         :kind vector?
                         :min-count 1
                         :into [])
    #(gen/vector (s/gen ::block-element) 1 2)))
(defnode ::section
  (s/keys :req-un [:spacetools.spacedoc.node.section/children]))


;; headline

(defmethod sc/headline-child :section [_] ::section)
(defmethod sc/headline-child :headline [_]
  :spacetools.spacedoc.headline*/descendent-headline)
(s/def ::headline-child (s/multi-spec sc/headline-child :tag))
(s/def :spacetools.spacedoc.node.headline/todo? boolean?)
(s/def :spacetools.spacedoc.node.headline/value
  :spacetools.spacedoc.node.val/non-blank-string)
(s/def :spacetools.spacedoc.node.headline/children
  (s/with-gen (s/coll-of ::headline-child
                         :kind vector?
                         :min-count 0
                         :distinct true
                         :into [])
    #(gen/vector-distinct (s/gen ::headline-child)
                          {:min-elements 1 :max-elements 2 :max-tries 100})))


(s/def :spacetools.spacedoc.headline*/base
  (s/keys :req-un [:spacetools.spacedoc.node.headline/tag
                   :spacetools.spacedoc.node.headline/value
                   :spacetools.spacedoc.node.headline/todo?
                   :spacetools.spacedoc.node.headline/children]))


(defn- headline?
  [node]
  (= (:tag node) :headline))


(defn- headline->depth
  "Return how deeply children of HEADLINE go."
  [headline]
  ((fn rec [depth node]
     (if (headline? node)
       (inc (r/reduce (r/monoid max (constantly 0))
                      (r/map (partial rec depth) (:children node))))
       depth))
   0 headline))


(defn- clamp-children
  "Delete HL headline children that are deeper than LEVEL."
  [level headline]
  ((fn rec [depth {:keys [value children todo?] :as node}]
     (assoc node :children
            (if (and (headline? node) (>= depth level))
              []
              (mapv (partial rec (inc depth)) children))))
   1 headline))


(defn- mark-empty-as-todo
  "Mark HEADLINE as todo if i doesn't have children."
  [headline]
  (if (empty? (:children headline))
    (assoc headline :todo? true)
    headline))


;; Handy in gen-testing headline constructors.
(s/def :spacetools.spacedoc.headline*/descendent-headline
  (s/and (s/with-gen :spacetools.spacedoc.headline*/base
           #(gen/fmap
             (comp mark-empty-as-todo
                   (partial clamp-children (dec (cfg/max-headline-depth))))
             (s/gen :spacetools.spacedoc.headline*/base)))
         (fn not-too-deep? [hl] (< (headline->depth hl)
                                  (dec (cfg/max-headline-depth))))
         (fn todo-or-has-children? [hl] (or (:todo? hl)
                                           (seq (:children hl))))))


;; Has to do it manually (for now):
(defmethod sc/node->spec-k :headline [_] ::headline)
(s/def :spacetools.spacedoc.node.headline/tag #{:headline})
;; TODO: Make it doable with `defnode` macro.
(s/def ::headline
  (s/and (s/with-gen :spacetools.spacedoc.headline*/base
           #(gen/fmap
             (comp mark-empty-as-todo
                   (partial clamp-children (cfg/max-headline-depth)))
             (s/gen :spacetools.spacedoc.headline*/base)))
         (fn not-too-deep? [hl] (< (headline->depth hl)
                                  (cfg/max-headline-depth)))
         (fn todo-or-has-children? [hl] (or (:todo? hl)
                                           (seq (:children hl))))))


;; root node

(defmethod sc/root-child :section [_] ::section)
(defmethod sc/root-child :headline [_] ::headline)
(s/def ::root-child (s/multi-spec sc/root-child :tag))
(s/def :spacetools.spacedoc.node.root/children
  (s/with-gen (s/coll-of ::root-child
                         :kind vector?
                         :distinct true
                         :into [])
    #(gen/vector-distinct (s/gen ::root-child)
                          {:min-elements 1 :max-elements 2})))
(s/def :spacetools.spacedoc.node.root/source
  :spacetools.spacedoc.node.val/non-blank-string)
(s/def :spacetools.spacedoc.node.root/spaceroot
  :spacetools.spacedoc.node.val/non-blank-string)
(defnode* ::root (s/keys :req-un [:spacetools.spacedoc.node.root/children]
                         :opt-un [:spacetools.spacedoc.node.root/source
                                  :spacetools.spacedoc.node.root/spaceroot]))


;;;; Meta specs

;; For the cases when we need a nonempty headline children vector.
(s/def :spacetools.spacedoc.node.meta.hl.nonempty/children
  (s/with-gen (s/coll-of ::headline-child
                         :kind vector?
                         :min-count 1
                         :distinct true
                         :into [])
    #(gen/vector-distinct (s/gen ::headline-child)
                          {:min-elements 1 :max-elements 2 :max-tries 100})))


;; Description meta node

(s/def :spacetools.spacedoc.node.meta.description/value #{"Description"})
(s/def :spacetools.spacedoc.node.meta.description/todo? false?)
(s/def :spacetools.spacedoc.node.meta/description
  (s/merge ::headline
           (s/keys
            :req-un [:spacetools.spacedoc.node.meta.description/todo?
                     :spacetools.spacedoc.node.meta.hl.nonempty/children])))


;; Todo meta node

(s/def :spacetools.spacedoc.node.meta.todo/todo? true?)
(s/def :spacetools.spacedoc.node.meta/todo
  (s/merge ::headline
           (s/keys :req-un [:spacetools.spacedoc.node.meta.todo/todo?])))


;;;; "handmade" human-friendly constructors

;; headline node

(defn-spec headline ::headline
  "\"headline\" node constructor."
  [value :spacetools.spacedoc.node.val/non-blank-string
   & children (s/with-gen (s/+ ::headline-child)
                #(gen/vector-distinct (s/gen ::headline-child)
                                      {:min-elements 1 :max-elements 3}))]
  {:pre [(s/valid? :spacetools.spacedoc.node.headline/value value)
         (s/valid? :spacetools.spacedoc.node.meta.hl.nonempty/children
                   (vec children))]
   :post [(s/valid? ::headline %)]}
  {:tag :headline :todo? false :value value :children (vec children)})


;; todo meta node

(defn-spec todo :spacetools.spacedoc.node.meta/todo
  "\"todo\" node constructor."
  [value :spacetools.spacedoc.node.val/non-blank-string
   & children (s/with-gen (s/* ::headline-child)
                #(gen/vector-distinct (s/gen ::headline-child)
                                      {:min-elements 0 :max-elements 3}))]
  {:pre [(s/valid? :spacetools.spacedoc.node.headline/value value)
         (s/valid? :spacetools.spacedoc.node.headline/children (vec children))]
   :post [(s/valid? :spacetools.spacedoc.node.meta/todo %)]}
  {:tag :headline :todo? true :value value :children (vec children)})


;; description meta node

(defn-spec description :spacetools.spacedoc.node.meta/description
  "\"description\" node constructor."
  [& children (s/with-gen (s/+ ::headline-child)
                #(gen/vector-distinct (s/gen ::headline-child)
                                      {:min-elements 1 :max-elements 3}))]
  {:pre [(s/valid? :spacetools.spacedoc.node.meta.hl.nonempty/children
                   (vec children))]
   :post [(s/valid? :spacetools.spacedoc.node.meta/description %)]}
  {:tag :headline :todo? false :value "Description" :children (vec children)})


;; link

(defn- link->link-prefix
  "Given full link return corresponding prefix(usually protocol + ://)."
  [path]
  (->> (vals (cfg/link-type->prefix))
       (filter (partial str/starts-with? path))
       (first)))


(defn-spec link ::link
  "\"link\" node constructor."
  [link :spacetools.spacedoc.node.link/path & children (s/* ::inline-element)]
  {:pre  [(link->link-prefix link)
          (s/valid? :spacetools.spacedoc.node.link/path link)
          (s/valid? :spacetools.spacedoc.node.link/children
                    (vec children))]
   :post [(s/valid? ::link %)]}
  (let [link-prefix (link->link-prefix link)
        link-type ((map-invert (cfg/link-type->prefix)) link-prefix)]
    {:tag :link
     :type link-type
     :path link
     :children (vec children)}))


;; plain-list

(defn- make-list-item
  "Create list item node."
  [item-type idx item]
  {:tag :list-item
   :type item-type
   :bullet (if (= item-type :unordered) "- " (str (inc idx) ". "))
   :checkbox nil
   :children [{:tag :item-children :children item}]})


(defn-spec unordered-list ::plain-list
  "Unordered \"plain-list\" node constructor.
  Usage:
  (unordered-list
    [(text \"foo\") (text \"bar\")]
    ...
    [(bold (text \"qux\"))])"
  [& items (s/with-gen (s/+ :spacetools.spacedoc.node.item-children/children)
             #(gen/vector
               (s/gen :spacetools.spacedoc.node.item-children/children) 1 3))]
  {:pre [(every? vector? items)]
   :post [(s/valid? ::plain-list %)]}
  {:tag :plain-list
   :type :unordered
   :children (into [] (map-indexed (partial make-list-item :unordered)) items)})


(defn-spec ordered-list ::plain-list
  "ordered \"plain-list\" node constructor.
  Usage:
  (ordered-list
   [(text \"foo\") (text \"bar\")]
   ...
   [(bold (text \"qux\"))])"
  [& items (s/with-gen (s/+ :spacetools.spacedoc.node.item-children/children)
             #(gen/vector
               (s/gen :spacetools.spacedoc.node.item-children/children) 1 3))]
  {:pre [(every? vector? items)]
   :post [(s/valid? ::plain-list %)]}
  {:tag :plain-list
   :type :ordered
   :children (into [] (map-indexed (partial make-list-item :ordered)) items)})


(defn-spec root ::root
  "\"root\" node constructor."
  [& children (s/with-gen (s/+ ::root-child)
                #(gen/vector-distinct (s/gen ::root-child)
                                      {:min-elements 1 :max-elements 3}))]
  {:pre [(s/valid? :spacetools.spacedoc.node.root/children (vec children))]
   :post [(s/valid? ::root %)]}
  {:tag :root :children (vec children)})


;; TODO: Add `:descriptive` list constructor.
;; `:descriptive` lists have `::list-item`s with optional `::item-tag`.
;; Arguments of the constructor can be something like this:
;; [<item-child>..]
;; ...
;; {<item-tag> [<item-child>...]} <- item-tag is a vector or string.
;; ...
;; [<item-child>...]

;; NOTE: We don't use checkboxes because GitHub doesn't support them
;;       But they can be added in a similar way to `::item-tag`s.

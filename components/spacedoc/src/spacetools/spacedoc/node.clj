(ns spacetools.spacedoc.node
  "Node specs and constructors. All public functions are constructors."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [map-invert]]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node-impl :refer [defnode defnode*]]
            [spacetools.spacedoc.node.util :as nu]
            [spacetools.spacedoc.node.val-spec :as vs]))


;; Any node should satisfy this spec

(s/def :spacetools.spacedoc.node.any-node/tag keyword?)
(s/def :spacetools.spacedoc.node.any-node/children (s/coll-of
                                                    ::any-node
                                                    :min-count 0
                                                    :kind vector?))
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
  (s/with-gen (s/coll-of ::vs/non-blank-string
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen ::vs/non-blank-string) 1 3)))


(defnode ::kbd (s/keys :req-un [:spacetools.spacedoc.node.kbd/value]))


;; line-break node

(defnode ::line-break (s/keys :req-un []))


;; text node

(s/def :spacetools.spacedoc.node.text/value string?)
(defnode ::text (s/keys :req-un [:spacetools.spacedoc.node.text/value]))


;; verbatim node

(s/def :spacetools.spacedoc.node.verbatim/value ::vs/non-blank-lines)
(defnode ::verbatim (s/keys :req-un [:spacetools.spacedoc.node.verbatim/value]))


;; inline element

(s/def ::inline-element (s/or :inline-container ::inline-container
                              :inline-leaf ::inline-leaf))


;; inline-container children

(s/def ::inline-container-children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen ::inline-element) 1 3)))


;; bold node

(s/def :spacetools.spacedoc.node.bold/children ::inline-container-children)
(defnode ::bold (s/keys :req-un [:spacetools.spacedoc.node.bold/children]))


;; italic node

(s/def :spacetools.spacedoc.node.italic/children ::inline-container-children)
(defnode ::italic (s/keys :req-un [:spacetools.spacedoc.node.italic/children]))


;; strike-through node

(s/def :spacetools.spacedoc.node.strike-through/children
  ::inline-container-children)
(defnode ::strike-through
  (s/keys :req-un [:spacetools.spacedoc.node.strike-through/children]))


;; underline node

(s/def :spacetools.spacedoc.node.underline/children
  ::inline-container-children)
(defnode ::underline
  (s/keys :req-un [:spacetools.spacedoc.node.underline/children]))


;; link
;; TODO:  It can be a problem that link type and path aren't synchronized.
;;        For example the generator will generate "file: foo" links for
;;        :custom-id type and it will be valid. Link spec should ensure
;;        internal consistency.
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
                         :min-count 0)
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
                         :min-count 1)
    #(gen/vector (s/gen ::paragraph-child) 1 3)))
(defnode ::paragraph
  (s/keys :req-un [:spacetools.spacedoc.node.paragraph/children]))


;; inline container

(defmethod sc/inline-container :bold [_] ::bold)
(defmethod sc/inline-container :italic [_] ::italic)
(defmethod sc/inline-container :link [_] ::link)
(defmethod sc/inline-container :strike-through [_] ::strike-through)
(defmethod sc/inline-container :underline [_] ::underline)
(s/def ::inline-container (s/multi-spec sc/inline-container :tag))


;; example node

(s/def :spacetools.spacedoc.node.example/value ::vs/non-blank-lines)
(defnode ::example (s/keys :req-un [:spacetools.spacedoc.node.example/value]))


;; item-children

(s/def :spacetools.spacedoc.node.item-children/child
  (s/or :block-element ::block-element
        :inline-element ::inline-element))
(s/def :spacetools.spacedoc.node.item-children/children
  (s/with-gen (s/coll-of :spacetools.spacedoc.node.item-children/child
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen :spacetools.spacedoc.node.item-children/child)
                 1 3)))
(defnode ::item-children
  (s/keys :req-un [:spacetools.spacedoc.node.item-children/children]))


;; item-tag

(s/def :spacetools.spacedoc.node.item-tag/child ::inline-element)
(s/def :spacetools.spacedoc.node.item-tag/children
  (s/with-gen (s/coll-of :spacetools.spacedoc.node.item-tag/child
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen :spacetools.spacedoc.node.item-tag/child) 1 3)))
(defnode ::item-tag (s/keys :req-un
                            [:spacetools.spacedoc.node.item-tag/children]))


;; list-item

(s/def :spacetools.spacedoc.node.list-item/type
  #{:ordered :unordered :descriptive})
(s/def :spacetools.spacedoc.node.list-item/bullet ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.list-item/checkbox
  (s/nilable #{:trans :off :on}))
(s/def :spacetools.spacedoc.node.list-item*/children-list
  (s/cat :children ::item-children
         :tag (s/? ::item-tag)))
(s/def :spacetools.spacedoc.node.list-item/children
  (s/with-gen :spacetools.spacedoc.node.list-item*/children-list
    #(gen/fmap vec (s/gen :spacetools.spacedoc.node.list-item*/children-list))))
(defnode* ::list-item (s/keys :req-un
                              [:spacetools.spacedoc.node.list-item/type
                               :spacetools.spacedoc.node.list-item/bullet
                               :spacetools.spacedoc.node.list-item/checkbox
                               :spacetools.spacedoc.node.list-item/children]))


;; plain-list node

(s/def :spacetools.spacedoc.node.plain-list/type
  #{:ordered :unordered :descriptive})
(s/def :spacetools.spacedoc.node.plain-list/children
  (s/with-gen (s/coll-of ::list-item
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen ::list-item) 1 3)))
(defnode* ::plain-list
  (s/keys :req-un [:spacetools.spacedoc.node.plain-list/type
                   :spacetools.spacedoc.node.plain-list/children]))


;; quoted node

(s/def :spacetools.spacedoc.node.quoted/children
  (s/with-gen (s/coll-of ::paragraph
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen ::paragraph) 1 3)))
(defnode ::quoted (s/keys :req-un [:spacetools.spacedoc.node.quoted/children]))


;; src node

(s/def :spacetools.spacedoc.node.src/language ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.src/value ::vs/non-blank-lines)
(defnode ::src (s/keys :req-un [:spacetools.spacedoc.node.src/language
                                :spacetools.spacedoc.node.src/value]))


;; table-cell node

(s/def :spacetools.spacedoc.node.table-cell/children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 0)
    #(gen/vector (s/gen ::inline-element) 0 3)))
(defnode* ::table-cell
  (s/keys :req-un [:spacetools.spacedoc.node.table-cell/children]))


;; table-row node

(s/def :spacetools.spacedoc.node.table-row/type #{:rule :standard})
(s/def :spacetools.spacedoc.node.table-row/children
  (s/coll-of ::table-cell
             :kind vector?
             :min-count 0))
(defnode* ::table-row
  (s/keys :req-un [:spacetools.spacedoc.node.table-row/type
                   :spacetools.spacedoc.node.table-row/children]))


;; table node

(s/def :spacetools.spacedoc.node.table/type #{:org})
(s/def :spacetools.spacedoc.node.table/children
  (s/with-gen (s/and (s/coll-of ::table-row
                                :kind vector?
                                :min-count 1)
                     (fn same-row-child-count?
                       [rows]
                       (let [lns (->> rows
                                      (remove #(#{:rule} (:type %)))
                                      (map #(count (:children %))))]
                         (every? (partial = (first lns)) lns))))
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
                 1 6)
                (gen/elements (range 1 3))))))
(defnode* ::table
  (s/keys :req-un [:spacetools.spacedoc.node.table/type
                   :spacetools.spacedoc.node.table/children]))


;; verse node

(s/def :spacetools.spacedoc.node.verse/children
  (s/with-gen (s/coll-of ::inline-element
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen ::inline-element) 1 3)))
(defnode ::verse (s/keys :req-un [:spacetools.spacedoc.node.verse/children]))


;; key-word node

(s/def :spacetools.spacedoc.node.key-word/key ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.key-word/value ::vs/non-blank-string)
(defnode ::key-word (s/keys :req-un [:spacetools.spacedoc.node.key-word/key
                                     :spacetools.spacedoc.node.key-word/value]))


;; block group

(defmethod sc/block-element :example [_] ::example)
(defmethod sc/block-element :paragraph [_] ::paragraph)
(defmethod sc/block-element :plain-list [_] ::plain-list)
(defmethod sc/block-element :quoted [_] ::quoted)
(defmethod sc/block-element :src [_] ::src)
(defmethod sc/block-element :table [_] ::table)
(defmethod sc/block-element :verse [_] ::verse)
(defmethod sc/block-element :key-word [_] ::key-word)
(s/def ::block-element (s/multi-spec sc/block-element :tag))


;; section node

(s/def :spacetools.spacedoc.node.section/children
  (s/with-gen (s/coll-of ::block-element
                         :kind vector?
                         :min-count 1)
    #(gen/vector (s/gen ::block-element) 1 2)))
(defnode ::section
  (s/keys :req-un [:spacetools.spacedoc.node.section/children]))


;; headline

(s/def :spacetools.spacedoc.node.headline/tag #{:headline})

(defmethod sc/headline-child :section [_] ::section)
(defmethod sc/headline-child :headline [_]
  :spacetools.spacedoc.headline*/descendent-headline)
(s/def ::headline-child (s/multi-spec sc/headline-child :tag))
(s/def :spacetools.spacedoc.node.headline/todo? boolean?)
(s/def :spacetools.spacedoc.node.headline/value ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.headline/children
  (s/with-gen (s/coll-of ::headline-child
                         :kind vector?
                         :min-count 0
                         :distinct true)
    #(gen/vector-distinct (s/gen ::headline-child)
                          {:min-elements 1 :max-elements 2 :max-tries 100})))

(s/def :spacetools.spacedoc.headline*/base
  (s/keys :req-un [:spacetools.spacedoc.node.headline/tag
                   :spacetools.spacedoc.node.headline/value
                   :spacetools.spacedoc.node.headline/todo?
                   :spacetools.spacedoc.node.headline/children]))

;; Handy in gen-testing headline constructors.
(s/def :spacetools.spacedoc.headline*/descendent-headline
  (s/and (s/with-gen :spacetools.spacedoc.headline*/base
           #(gen/fmap (fn [hl]
                        (nu/fmt-headline (dec (cfg/max-headline-depth)) hl))
                      (s/gen :spacetools.spacedoc.headline*/base)))
         #(<= (nu/headline->depth %) (dec (cfg/max-headline-depth)))
         nu/todo-or-has-children?))


;; Has to do it manually (for now):
(defmethod sc/node->spec-k :headline [_] ::headline)
;; TODO: Make it doable with `defnode` macro.
(s/def ::headline
  (s/and (s/with-gen :spacetools.spacedoc.headline*/base
           #(gen/fmap (fn [hl]
                        (nu/fmt-headline (cfg/max-headline-depth) hl))
                      (s/gen :spacetools.spacedoc.headline*/base)))
         #(<= (nu/headline->depth %) (cfg/max-headline-depth))
         nu/todo-or-has-children?))


;; root node

(defmethod sc/root-child :section [_] ::section)
(defmethod sc/root-child :headline [_] ::headline)
(s/def ::root-child (s/multi-spec sc/root-child :tag))
(s/def :spacetools.spacedoc.node.root/children
  (s/with-gen (s/coll-of ::root-child
                         :min-count 1
                         :kind vector?
                         :distinct true)
    #(gen/vector-distinct (s/gen ::root-child)
                          {:min-elements 1 :max-elements 2})))
(s/def :spacetools.spacedoc.node.root/title ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.root/tags
  (s/with-gen
    (s/and
     (s/coll-of ::vs/non-blank-string
                :kind set?)
     ;; NOTE: We have to prevent this from inlining
     #(set/superset? (cfg/valid-tags) %))
    #(->> (cfg/valid-tags)
          keys
          gen/elements
          gen/vector-distinct
          (gen/fmap set))))
(s/def :spacetools.spacedoc.node.root/source ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.root/root-dir ::vs/non-blank-string)
(defnode* ::root (s/keys :req-un [:spacetools.spacedoc.node.root/title
                                  :spacetools.spacedoc.node.root/tags
                                  :spacetools.spacedoc.node.root/children]
                         :opt-un [:spacetools.spacedoc.node.root/source
                                  :spacetools.spacedoc.node.root/root-dir]))


;;;; Meta specs

;; Empty root node

(s/def :spacetools.spacedoc.node.meta.empty-root/children #{[]})
(s/def :spacetools.spacedoc.node.meta/empty-root
  (s/keys :req-un [:spacetools.spacedoc.node.root/title
                   :spacetools.spacedoc.node.root/tags
                   :spacetools.spacedoc.node.meta.empty-root/children]
          :opt-un [:spacetools.spacedoc.node.root/source
                   :spacetools.spacedoc.node.root/root-dir]))

;; For the cases when we need a nonempty headline children vector.
(s/def :spacetools.spacedoc.node.meta.hl.nonempty/children
  (s/coll-of ::headline-child
             :kind vector?
             :min-count 1
             :distinct true))


;; Description meta node

(s/def :spacetools.spacedoc.node.meta.description/value #{"Description"})
(s/def :spacetools.spacedoc.node.meta.description/todo? false?)
(s/def :spacetools.spacedoc.node.meta/description
  (s/merge ::headline
           (s/keys
            :req-un [:spacetools.spacedoc.node.meta.description/value
                     :spacetools.spacedoc.node.meta.description/todo?
                     :spacetools.spacedoc.node.meta.hl.nonempty/children])))


;; Todo meta node

(s/def :spacetools.spacedoc.node.meta.todo/todo? true?)
(s/def :spacetools.spacedoc.node.meta/todo
  (s/merge ::headline
           (s/keys :req-un [:spacetools.spacedoc.node.meta.todo/todo?])))


;; title meta node

(s/def :spacetools.spacedoc.node.meta.title/key
  (s/with-gen (s/and string? (partial re-matches #"(?i)title"))
    ;; FIXME: Randomize the generated string case
    #(gen/elements ["title" "Title" "TITLE"])))
(s/def :spacetools.spacedoc.node.meta.title/value ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.meta/title
  (s/keys :req-un [:spacetools.spacedoc.node.meta.title/key
                   :spacetools.spacedoc.node.meta.title/value]))


;; tags meta node

(s/def :spacetools.spacedoc.node.meta.tags/key
  (s/with-gen (s/and string? (partial re-matches #"(?i)tags"))
    ;; FIXME: Randomize the generated string case
    #(gen/elements ["tags" "Tags" "TAGS"])))
(s/def :spacetools.spacedoc.node.meta.tags/value ::vs/non-blank-string)
(s/def :spacetools.spacedoc.node.meta/tags
  (s/keys :req-un [:spacetools.spacedoc.node.meta.tags/key
                   :spacetools.spacedoc.node.meta.tags/value]))


;;;; "handmade" human-friendly constructors

;; headline node constructor

(defn-spec headline ::headline
  "\"headline\" node constructor."
  [value ::vs/non-blank-string
   & children (s/with-gen (s/+ ::headline-child)
                #(gen/vector-distinct (s/gen ::headline-child)
                                      {:min-elements 1 :max-elements 3}))]
  {:tag :headline :todo? false :value value :children (vec children)})


;; todo node constructor

(defn-spec todo :spacetools.spacedoc.node.meta/todo
  "\"todo\" node constructor."
  [value ::vs/non-blank-string
   & children (s/with-gen (s/* ::headline-child)
                #(gen/vector-distinct (s/gen ::headline-child)
                                      {:min-elements 0 :max-elements 3}))]
  {:tag :headline :todo? true :value value :children (vec children)})


;; description node constructor

(defn-spec description :spacetools.spacedoc.node.meta/description
  "\"description\" node constructor."
  [& children (s/with-gen (s/+ ::headline-child)
                #(gen/vector-distinct (s/gen ::headline-child)
                                      {:min-elements 1 :max-elements 3}))]
  {:tag :headline :todo? false :value "Description" :children (vec children)})


;; link node constructor

(defn-spec link ::link
  "\"link\" node constructor."
  [link :spacetools.spacedoc.node.link/path & children (s/* ::inline-element)]
  (let [link-prefix (->> (vals (cfg/link-type->prefix))
                         (filter (partial str/starts-with? link))
                         (first))
        link-type ((map-invert (cfg/link-type->prefix)) link-prefix)]
    {:tag :link
     :type link-type
     :path link
     :children (vec children)}))


;; list-item node constructor

(defn-spec list-item ::list-item
  "create list item node."
  [item-type #{:ordered :unordered}
   idx nat-int?
   & children (s/with-gen (s/+ :spacetools.spacedoc.node.item-children/child)
                #(gen/vector
                  (s/gen :spacetools.spacedoc.node.item-children/child)
                  1 3))]
  {:tag :list-item
   :type item-type
   :bullet (if (= item-type :unordered) "- " (str (inc idx) ". "))
   :checkbox nil
   :children [{:tag :item-children :children (vec children)}]})


;; plain-list node constructors

(defn-spec unordered-list ::plain-list
  "Unordered \"plain-list\" node constructor.
  Usage:
  (unordered-list
    [(text \"foo\") (text \"bar\")]
    ...
    [(bold (text \"qux\"))])"
  [& item-children
   (s/with-gen (s/+ :spacetools.spacedoc.node.item-children/children)
     #(gen/vector
       (s/gen :spacetools.spacedoc.node.item-children/children) 1 3))]
  {:tag :plain-list
   :type :unordered
   :children (vec (map-indexed (partial apply list-item :unordered)
                               item-children))})


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
  {:tag :plain-list
   :type :ordered
   :children (vec (map-indexed (partial apply list-item :ordered) items))})


;; root node constructor

(defn-spec root ::root
  "\"root\" node constructor."
  [title :spacetools.spacedoc.node.root/title
   tags :spacetools.spacedoc.node.root/tags
   & children (s/with-gen (s/+ ::root-child)
                #(gen/vector-distinct (s/gen ::root-child)
                                      {:min-elements 1 :max-elements 3}))]
  {:tag :root :title title :tags tags :children (vec children)})


(s/fdef table
  :args (s/with-gen (s/and
                     (s/+ (s/coll-of (s/coll-of ::inline-element
                                                :min-count 1)))
                     (fn square-table? [rows]
                       (if-let [no-rule (seq (remove empty? rows))]
                         (apply = (map count no-rule))
                         true)))
          #(-> ::inline-element
               s/gen
               (gen/vector 1 3)
               (gen/vector 0 3)
               (gen/vector 1 3)))
  :ret ::table)


;; table node constructor

(defn table
  "\"table\" node constructor."
  [& rows]
  {:tag :table
   :type :org
   :children (r/reduce
              (r/monoid conj vector)
              (r/map (fn [cell]
                       (merge {:tag :table-row
                               :children []
                               :type :rule}
                              (when (seq cell)
                                {:type :standard
                                 :children cell})))
                     (r/map (fn [cell]
                              (mapv (fn [cont] {:tag :table-cell
                                               :children cont})
                                    cell))
                            rows)))})


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

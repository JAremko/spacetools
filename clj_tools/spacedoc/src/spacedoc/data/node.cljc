(ns ^{:doc "Shared SDN node generators.
All public function in this name-space are node constructors.
NOTE: Format specific specs are in corresponding name-spaces.
EXAMPLE: :spacedoc.data.org/toc"}
    spacedoc.data.node
  (:require [clojure.set :refer [union map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacedoc.data :as data]
            [spacedoc.data.node-impl :refer [defnode]]))


;; (headline* "foo" 1 "foo" (section (paragraph (text "s"))))

;; (require '[clojure.spec.test.alpha :as stest])

;; (stest/instrument `list-item)

;; (list-item :ordered nil "+" (item-children (text "ss")) (item-tag (text "ss")))


;;;; Constructors

(defn unordered-list
  "Unordered \"plain-list\" node constructor."
  [items])


(defn ordered-list
  "ordered \"plain-list\" node constructor."
  [items])


(defn link
  "\"link\" node constructor."
  [path & children]
  {:pre  [(data/path->link-prefix path)]}
  (let [link-prefix (data/path->link-prefix path)
        link-type ((map-invert data/link-type->prefix) link-prefix)]
    {:tag :link
     :path (str/replace-first path link-prefix "")
     :type link-type
     :raw-link path
     :children (vec children)}))


;;;; Nodes definitions via specs


;; Shared specs


;; NOTE: Actually some lines may be empty but not all of them.
(s/def ::has-non-empty-line
  (s/and string? #(re-matches #"^(?:.+\n*.*|.*\n*.+|\n*.+\n*)+$" %)))


(s/def ::non-empty-string (s/and string? #(re-matches #"^.+$" %)))


(s/def ::any any?)


;; inline leaf

(defmulti ^:private  inline-leaf :tag)
(defmethod inline-leaf :kbd [_] ::kbd)
(defmethod inline-leaf :line-break [_] ::line-break)
(defmethod inline-leaf :text [_] ::text)
(defmethod inline-leaf :verbatim [_] ::verbatim)
(s/def ::inline-leaf (s/multi-spec inline-leaf :tag))

(def inline-leaf-tags (set (keys (methods inline-leaf))))


;; kbd node

(s/def :spacedoc.data.node.kbd/value (s/coll-of ::non-empty-string
                                                :kind vector?
                                                :min-count 1
                                                :into []))


(defnode ::kbd (s/keys :req-un [:spacedoc.data.node.kbd/value]))


;; line-break node

(defnode ::line-break (s/keys :req-un []))


;; text node

(s/def :spacedoc.data.node.text/value string?)
(defnode ::text (s/keys :req-un [:spacedoc.data.node.text/value]))


;; verbatim node

(s/def :spacedoc.data.node.verbatim/value ::has-non-empty-line)
(defnode ::verbatim (s/keys :req-un [:spacedoc.data.node.verbatim/value]))


;; inline element

(s/def ::inline-element (s/or :inline-container ::inline-container
                              :inline-leaf ::inline-leaf))


;; inline-container children

(s/def ::inline-container-children (s/coll-of ::inline-element
                                              :kind vector?
                                              :min-count 1
                                              :into []))


;; bold node

(s/def :spacedoc.data.node.bold/children ::inline-container-children)
(defnode ::bold (s/keys :req-un [:spacedoc.data.node.bold/children]))


;; italic node

(s/def :spacedoc.data.node.italic/children ::inline-container-children)
(defnode ::italic (s/keys :req-un [:spacedoc.data.node.italic/children]))


;; strike-through node

(s/def :spacedoc.data.node.strike-through/children ::inline-container-children)
(defnode ::strike-through
  (s/keys :req-un [:spacedoc.data.node.strike-through/children]))


;; subscript node

(s/def :spacedoc.data.node.subscript/children ::inline-container-children)
(defnode ::subscript (s/keys :req-un [:spacedoc.data.node.subscript/children]))


;; superscript node

(s/def :spacedoc.data.node.superscript/children ::inline-container-children)
(defnode ::superscript
  (s/keys :req-un [:spacedoc.data.node.superscript/children]))


;; underline node

(s/def :spacedoc.data.node.underline/children ::inline-container-children)
(defnode ::underline (s/keys :req-un [:spacedoc.data.node.underline/children]))


;; link

(s/def :spacedoc.data.node.link/path ::non-empty-string)
(s/def :spacedoc.data.node.link/type (set (keys data/link-type->prefix)))
(s/def :spacedoc.data.node.link/raw-link ::non-empty-string)
(s/def :spacedoc.data.node.link/children (s/coll-of ::inline-element
                                                    :kind vector?
                                                    :into []))
(defnode ::link "`link`" (s/keys :req-un [:spacedoc.data.node.link/path
                                          :spacedoc.data.node.link/type
                                          :spacedoc.data.node.link/raw-link
                                          :spacedoc.data.node.link/children]))


;; paragraph node

(s/def ::paragraph-child ::inline-element)
(s/def :spacedoc.data.node.paragraph/children (s/coll-of ::paragraph-child
                                                         :kind vector?
                                                         :min-count 1
                                                         :into []))
(defnode ::paragraph (s/keys :req-un [:spacedoc.data.node.paragraph/children]))


;; inline container

(defmulti ^:private  inline-container :tag)
(defmethod inline-container :bold [_] ::bold)
(defmethod inline-container :italic [_] ::italic)
(defmethod inline-container :link [_] ::link)
(defmethod inline-container :strike-through [_] ::strike-through)
(defmethod inline-container :subscript [_] ::subscript)
(defmethod inline-container :superscript [_] ::superscript)
(defmethod inline-container :paragraph [_] ::paragraph)
(defmethod inline-container :underline [_] ::underline)
(s/def ::inline-container (s/multi-spec inline-container :tag))

(def inline-container-tags (set (keys (methods inline-container))))
(def inline-tags (set (union inline-leaf-tags inline-container-tags)))


;; center node

(s/def ::center-child ::inline-element)
(s/def :spacedoc.data.node.center/children (s/coll-of ::center-child
                                                      :kind vector?
                                                      :min-count 1
                                                      :into []))
(defnode ::center (s/keys :req-un [:spacedoc.data.node.center/children]))


;; example node

(s/def :spacedoc.data.node.example/value ::has-non-empty-line)
(defnode ::example (s/keys :req-un [:spacedoc.data.node.example/value]))


;; item-children

(s/def :spacedoc.data.node.item-children/child
  (s/or :block-element ::block-element
        :inline-element ::inline-element))
(s/def :spacedoc.data.node.item-children/children
  (s/coll-of :spacedoc.data.node.item-children/child
             :kind vector?
             :min-count 1
             :into []))
(defnode ::item-children
  (s/keys :req-un [:spacedoc.data.node.item-children/children]))


;; item-tag

(s/def :spacedoc.data.node.item-tag/child ::inline-element)
(s/def :spacedoc.data.node.item-tag/children
  (s/coll-of :spacedoc.data.node.item-tag/child
             :kind vector?
             :min-count 1
             :into []))
(defnode ::item-tag (s/keys :req-un [:spacedoc.data.node.item-tag/children]))


;; list-item

(s/def :spacedoc.data.node.list-item/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.node.list-item/bullet ::non-empty-string)
(s/def :spacedoc.data.node.list-item/checkbox (s/nilable #{:trans :off :on}))
(s/def :spacedoc.data.node.list-item/children (s/cat :children ::item-children
                                                     :tag (s/? ::item-tag)))
(defnode ::list-item (s/keys :req-un [:spacedoc.data.node.list-item/type
                                      :spacedoc.data.node.list-item/bullet
                                      :spacedoc.data.node.list-item/checkbox
                                      :spacedoc.data.node.list-item/children]))


;; feature-list node

(s/def :spacedoc.data.node.feature-list/type
  #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.node.feature-list/children (s/coll-of ::list-item
                                                            :kind vector?
                                                            :min-count 1
                                                            :into []))
(defnode ::feature-list
  (s/keys :req-un [:spacedoc.data.node.feature-list/type
                   :spacedoc.data.node.feature-list/children]))


;; plain-list node

(s/def :spacedoc.data.node.plain-list/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.node.plain-list/children (s/coll-of ::list-item
                                                          :kind vector?
                                                          :min-count 1
                                                          :into []))
(defnode ::plain-list "`ordered-list` and `unordered-list`."
  (s/keys :req-un [:spacedoc.data.node.plain-list/type
                   :spacedoc.data.node.plain-list/children]))


;; quote node

(s/def :spacedoc.data.node.quote/children (s/coll-of ::paragraph
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::quote (s/keys :req-un [:spacedoc.data.node.quote/children]))


;; src node

(s/def :spacedoc.data.node.src/language ::non-empty-string)
(s/def :spacedoc.data.node.src/value ::has-non-empty-line)
(defnode ::src (s/keys :req-un [:spacedoc.data.node.src/language
                                :spacedoc.data.node.src/value]))


;; table-row node

(s/def :spacedoc.data.node.table-row/type #{:rule :standard})
(s/def :spacedoc.data.node.table-row/children (s/coll-of ::table-cell
                                                         :kind vector?
                                                         :min-count 0
                                                         :into []))
(defnode ::table-row (s/keys :req-un [:spacedoc.data.node.table-row/type
                                      :spacedoc.data.node.table-row/children]))


;; table-cell node

(s/def :spacedoc.data.node.table-cell/children (s/coll-of ::inline-element
                                                          :kind vector?
                                                          :min-count 0
                                                          :into []))
(defnode ::table-cell
  (s/keys :req-un [:spacedoc.data.node.table-cell/children]))


;; table node

(s/def :spacedoc.data.node.table/type #{:org})
(s/def :spacedoc.data.node.table/children (s/coll-of ::table-row
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::table "`table`"
  (s/keys :req-un [:spacedoc.data.node.table/type
                   :spacedoc.data.node.table/children]))


;; verse node

(s/def :spacedoc.data.node.verse/children (s/coll-of ::inline-element
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::verse (s/keys :req-un [:spacedoc.data.node.verse/children]))


;; key-word node

(s/def :spacedoc.data.node.key-word/key ::non-empty-string)
(s/def :spacedoc.data.node.key-word/value ::non-empty-string)
(defnode ::key-word (s/keys :req-un [:spacedoc.data.node.key-word/key
                                     :spacedoc.data.node.key-word/value]))


;; block group

(defmulti ^:private  block-element :tag)
(defmethod block-element :center [_] ::center)
(defmethod block-element :example [_] ::example)
(defmethod block-element :paragraph [_] ::paragraph)
(defmethod block-element :feature-list [_] ::feature-list)
(defmethod block-element :plain-list [_] ::plain-list)
(defmethod block-element :quote [_] ::quote)
(defmethod block-element :src [_] ::src)
(defmethod block-element :table [_] ::table)
(defmethod block-element :verse [_] ::verse)
(defmethod block-element :key-word [_] ::key-word)
(s/def ::block-element (s/multi-spec block-element :tag))

(def block-tags (set (keys (methods block-element))))


;; section node

(s/def :spacedoc.data.node.section/children (s/coll-of ::block-element
                                                       :kind vector?
                                                       :min-count 1
                                                       :into []))
(defnode ::section (s/keys :req-un [:spacedoc.data.node.section/children]))


;;;; Headlines

(def headline-tags #{:description :todo :headline})


(defmulti ^:private headline-child :tag)
(defmethod headline-child :todo [_] ::todo)
(defmethod headline-child :section [_] ::section)
(defmethod headline-child :headline [_] ::headline)
(s/def ::headline-child (s/multi-spec headline-child :tag))


;; headline

(s/def :spacedoc.data.node.headline/value ::non-empty-string)
(s/def :spacedoc.data.node.headline/path-id data/path-id?)
(s/def :spacedoc.data.node.headline/level
  (set (range 1 (inc data/max-headline-depth))))
(s/def :spacedoc.data.node.headline/children (s/coll-of ::headline-child
                                                        :kind vector?
                                                        :min-count 1
                                                        :distinct true
                                                        :into []))
(defnode ::headline "`headline`"
  (s/keys :req-un [:spacedoc.data.node.headline/value
                   :spacedoc.data.node.headline/children]
          :opt-un [:spacedoc.data.node.headline/level
                   :spacedoc.data.node.headline/path-id]))


;; description node

(s/def :spacedoc.data.node.description/value #{"Description"})
(s/def :spacedoc.data.node.description/path-id #{"description"})
(s/def :spacedoc.data.node.description/level #{1})
(s/def :spacedoc.data.node.description/children
  :spacedoc.data.node.headline/children)
(defnode ::description "`description`"
  (s/keys :req-un [:spacedoc.data.node.description/value
                   :spacedoc.data.node.description/path-id
                   :spacedoc.data.node.description/level
                   :spacedoc.data.node.description/children]))


;; todo node

(s/def :spacedoc.data.node.todo/value ::non-empty-string)
(s/def :spacedoc.data.node.todo/path-id data/path-id?)
(s/def :spacedoc.data.node.todo/level
  (set (range 1 (inc data/max-headline-depth))))
(s/def :spacedoc.data.node.todo/children (s/coll-of ::headline-child
                                                    :kind vector?
                                                    :into []))
(defnode ::todo "`todo`"
  (s/keys :req-un [:spacedoc.data.node.todo/value]
          :opt-un [:spacedoc.data.node.todo/level
                   :spacedoc.data.node.todo/path-id
                   :spacedoc.data.node.todo/children]))


;; root node

(defmulti ^:private  root-child :tag)
(defmethod root-child :todo [_] ::todo)
(defmethod root-child :section [_] ::section)
(defmethod root-child :headline [_] ::headline)
(defmethod root-child :description [_] ::description)
(s/def ::root-child (s/multi-spec root-child :tag))
(s/def :spacedoc.data.node.root/children (s/coll-of ::root-child
                                                    :kind vector?
                                                    :min-count 1
                                                    :distinct true
                                                    :into []))
(defnode ::root (s/keys :req-un [:spacedoc.data.node.root/children]))

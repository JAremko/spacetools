(ns ^{:doc "Shared SDN node generators.
All public function in this name-space are node constructors.
NOTE: Format specific specs are in corresponding name-spaces.
EXAMPLE: :spacedoc.data.org.toc"}
    spacedoc.data.node
  (:require [spacedoc.data :refer [defnode path-id?]]
            [spacedoc.data :as data]
            [clojure.set :refer [union map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))


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
  (s/and string?
         #(re-matches #"^(?:.+\n*.*|.*\n*.+|\n*.+\n*)+$" %)))


(s/def ::non-empty-string (s/and string?
                                 #(re-matches #"^.+$" %)))


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

(s/def :spacedoc.data.kbd/tag #{:kbd})
(s/def :spacedoc.data.kbd/value (s/coll-of ::non-empty-string
                                           :kind vector?
                                           :min-count 1
                                           :into []))


(defnode ::kbd (s/keys :req-un [:spacedoc.data.kbd/tag
                                :spacedoc.data.kbd/value]))


;; line-break node

(s/def :spacedoc.data.line-break/tag #{:line-break})
(defnode ::line-break (s/keys :req-un [:spacedoc.data.line-break/tag]))


;; text node

(s/def :spacedoc.data.text/tag #{:text})
(s/def :spacedoc.data.text/value string?)
(defnode ::text (s/keys :req-un [:spacedoc.data.text/tag
                                 :spacedoc.data.text/value]))


;; verbatim node

(s/def :spacedoc.data.verbatim/tag #{:verbatim})
(s/def :spacedoc.data.verbatim/value ::has-non-empty-line)
(defnode ::verbatim (s/keys :req-un [:spacedoc.data.verbatim/tag
                                     :spacedoc.data.verbatim/value]))


;; inline element

(s/def ::inline-element (s/or :inline-container ::inline-container
                              :inline-leaf ::inline-leaf))


;; inline-container children

(s/def ::inline-container-children (s/coll-of ::inline-element
                                              :kind vector?
                                              :min-count 1
                                              :into []))


;; bold node

(s/def :spacedoc.data.bold/tag #{:bold})
(s/def :spacedoc.data.bold/children ::inline-container-children)
(defnode ::bold (s/keys :req-un [:spacedoc.data.bold/tag
                                 :spacedoc.data.bold/children]))


;; italic node

(s/def :spacedoc.data.italic/tag #{:italic})
(s/def :spacedoc.data.italic/children ::inline-container-children)
(defnode ::italic (s/keys :req-un [:spacedoc.data.italic/tag
                                   :spacedoc.data.italic/children]))


;; strike-through node

(s/def :spacedoc.data.strike-through/tag #{:strike-through})
(s/def :spacedoc.data.strike-through/children ::inline-container-children)
(defnode ::strike-through (s/keys :req-un
                                  [:spacedoc.data.strike-through/tag
                                   :spacedoc.data.strike-through/children]))


;; subscript node

(s/def :spacedoc.data.subscript/tag #{:subscript})
(s/def :spacedoc.data.subscript/children ::inline-container-children)
(defnode ::subscript (s/keys :req-un [:spacedoc.data.subscript/tag
                                      :spacedoc.data.subscript/children]))


;; superscript node

(s/def :spacedoc.data.superscript/tag #{:superscript})
(s/def :spacedoc.data.superscript/children ::inline-container-children)
(defnode ::superscript (s/keys :req-un [:spacedoc.data.superscript/tag
                                        :spacedoc.data.superscript/children]))


;; underline node

(s/def :spacedoc.data.underline/tag #{:underline})
(s/def :spacedoc.data.underline/children ::inline-container-children)
(defnode ::underline (s/keys :req-un [:spacedoc.data.underline/tag
                                      :spacedoc.data.underline/children]))


;; link

(s/def :spacedoc.data.link/tag #{:link})
(s/def :spacedoc.data.link/path ::non-empty-string)
(s/def :spacedoc.data.link/type (set (keys data/link-type->prefix)))
(s/def :spacedoc.data.link/raw-link ::non-empty-string)
(s/def :spacedoc.data.link/children (s/coll-of ::inline-element
                                               :kind vector?
                                               :into []))
(defnode ::link (s/keys :req-un [:spacedoc.data.link/tag
                                 :spacedoc.data.link/path
                                 :spacedoc.data.link/type
                                 :spacedoc.data.link/raw-link
                                 :spacedoc.data.link/children]))


;; paragraph node

(s/def :spacedoc.data.paragraph/tag #{:paragraph})
(s/def ::paragraph-child ::inline-element)
(s/def :spacedoc.data.paragraph/children (s/coll-of ::paragraph-child
                                                    :kind vector?
                                                    :min-count 1
                                                    :into []))
(defnode ::paragraph (s/keys :req-un [:spacedoc.data.paragraph/tag
                                      :spacedoc.data.paragraph/children]))


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

(s/def :spacedoc.data.center/tag #{:center})
(s/def ::center-child ::inline-element)
(s/def :spacedoc.data.center/children (s/coll-of ::center-child
                                                 :kind vector?
                                                 :min-count 1
                                                 :into []))
(defnode ::center (s/keys :req-un [:spacedoc.data.center/tag
                                   :spacedoc.data.center/children]))


;; example node

(s/def :spacedoc.data.example/tag #{:example})
(s/def :spacedoc.data.example/value ::has-non-empty-line)
(defnode ::example (s/keys :req-un [:spacedoc.data.example/tag
                                    :spacedoc.data.example/value]))


;; item-children

(s/def :spacedoc.data.item-children/tag #{:item-children})
(s/def :spacedoc.data.item-children/child
  (s/or :block-element ::block-element
        :inline-element ::inline-element))
(s/def :spacedoc.data.item-children/children
  (s/coll-of :spacedoc.data.item-children/child
             :kind vector?
             :min-count 1
             :into []))
(defnode ::item-children
  (s/keys :req-un [:spacedoc.data.item-children/tag
                   :spacedoc.data.item-children/children]))


;; item-tag

(s/def :spacedoc.data.item-tag/tag #{:item-tag})
(s/def :spacedoc.data.item-tag/child ::inline-element)
(s/def :spacedoc.data.item-tag/children
  (s/coll-of :spacedoc.data.item-tag/child
             :kind vector?
             :min-count 1
             :into []))
(defnode ::item-tag (s/keys :req-un [:spacedoc.data.item-tag/tag
                                     :spacedoc.data.item-tag/children]))


;; list-item

(s/def :spacedoc.data.list-item/tag #{:list-item})
(s/def :spacedoc.data.list-item/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.list-item/bullet ::non-empty-string)
(s/def :spacedoc.data.list-item/checkbox (s/nilable #{:trans :off :on}))
(s/def :spacedoc.data.list-item/children (s/cat :children ::item-children
                                                :tag (s/? ::item-tag)))
(defnode ::list-item (s/keys :req-un [:spacedoc.data.list-item/tag
                                      :spacedoc.data.list-item/type
                                      :spacedoc.data.list-item/bullet
                                      :spacedoc.data.list-item/checkbox
                                      :spacedoc.data.list-item/children]))


;; feature-list node

(s/def :spacedoc.data.feature-list/tag #{:feature-list})
(s/def :spacedoc.data.feature-list/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.feature-list/children (s/coll-of ::list-item
                                                       :kind vector?
                                                       :min-count 1
                                                       :into []))
(defnode ::feature-list (s/keys :req-un [:spacedoc.data.feature-list/tag
                                         :spacedoc.data.feature-list/type
                                         :spacedoc.data.feature-list/children]))


;; plain-list node

(s/def :spacedoc.data.plain-list/tag #{:plain-list})
(s/def :spacedoc.data.plain-list/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.plain-list/children (s/coll-of ::list-item
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::plain-list (s/keys :req-un [:spacedoc.data.plain-list/tag
                                       :spacedoc.data.plain-list/type
                                       :spacedoc.data.plain-list/children]))


;; quote node

(s/def :spacedoc.data.quote/tag #{:quote})
(s/def :spacedoc.data.quote/children (s/coll-of ::paragraph
                                                :kind vector?
                                                :min-count 1
                                                :into []))
(defnode ::quote (s/keys :req-un [:spacedoc.data.quote/tag
                                  :spacedoc.data.quote/children]))


;; src node

(s/def :spacedoc.data.src/tag #{:src})
(s/def :spacedoc.data.src/language ::non-empty-string)
(s/def :spacedoc.data.src/value ::has-non-empty-line)
(defnode ::src (s/keys :req-un [:spacedoc.data.src/tag
                                :spacedoc.data.src/language
                                :spacedoc.data.src/value]))


;; table-row node

(s/def :spacedoc.data.table-row/tag #{:table-row})
(s/def :spacedoc.data.table-row/type #{:rule :standard})
(s/def :spacedoc.data.table-row/children (s/coll-of ::table-cell
                                                    :kind vector?
                                                    :min-count 0
                                                    :into []))
(defnode ::table-row (s/keys :req-un [:spacedoc.data.table-row/tag
                                      :spacedoc.data.table-row/type
                                      :spacedoc.data.table-row/children]))


;; table-cell node

(s/def :spacedoc.data.table-cell/tag #{:table-cell})
(s/def :spacedoc.data.table-cell/children (s/coll-of ::inline-element
                                                     :kind vector?
                                                     :min-count 0
                                                     :into []))
(defnode ::table-cell (s/keys :req-un [:spacedoc.data.table-cell/tag
                                       :spacedoc.data.table-cell/children]))


;; table node

(s/def :spacedoc.data.table/tag #{:table})
(s/def :spacedoc.data.table/type #{:org})
(s/def :spacedoc.data.table/children (s/coll-of ::table-row
                                                :kind vector?
                                                :min-count 1
                                                :into []))
(defnode ::table (s/keys :req-un [:spacedoc.data.table/tag
                                  :spacedoc.data.table/type
                                  :spacedoc.data.table/children]))


;; verse node

(s/def :spacedoc.data.verse/tag #{:verse})
(s/def :spacedoc.data.verse/children (s/coll-of ::inline-element
                                                :kind vector?
                                                :min-count 1
                                                :into []))
(defnode ::verse (s/keys :req-un [:spacedoc.data.verse/tag
                                  :spacedoc.data.verse/children]))


;; key-word node

(s/def :spacedoc.data.key-word/tag #{:key-word})
(s/def :spacedoc.data.key-word/key ::non-empty-string)
(s/def :spacedoc.data.key-word/value ::non-empty-string)
(defnode ::key-word (s/keys :req-un [:spacedoc.data.key-word/tag
                                     :spacedoc.data.key-word/key
                                     :spacedoc.data.key-word/value]))


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

(s/def :spacedoc.data.section/tag #{:section})
(s/def :spacedoc.data.section/children (s/coll-of ::block-element
                                                  :kind vector?
                                                  :min-count 1
                                                  :into []))
(defnode ::section (s/keys :req-un [:spacedoc.data.section/tag
                                    :spacedoc.data.section/children]))


;;;; Headlines

(def headline-tags #{:description :todo :headline})


(defmulti ^:private headline-child :tag)
(defmethod headline-child :todo [_] ::todo)
(defmethod headline-child :section [_] ::section)
(defmethod headline-child :headline [_] ::headline)
(s/def ::headline-child (s/multi-spec headline-child :tag))


;; headline

(s/def :spacedoc.data.headline/tag #{:headline})
(s/def :spacedoc.data.headline/value ::non-empty-string)
(s/def :spacedoc.data.headline/path-id path-id?)
(s/def :spacedoc.data.headline/level
  (set (range 1 (inc data/max-headline-depth))))
(s/def :spacedoc.data.headline/children (s/coll-of ::headline-child
                                                   :kind vector?
                                                   :min-count 1
                                                   :distinct true
                                                   :into []))
(defnode ::headline
  (s/keys :req-un [:spacedoc.data.headline/tag
                   :spacedoc.data.headline/value
                   :spacedoc.data.headline/children]
          :opt-un [:spacedoc.data.headline/level
                   :spacedoc.data.headline/path-id]))


;; description node

(s/def :spacedoc.data.description/tag #{:description})
(s/def :spacedoc.data.description/value #{"Description"})
(s/def :spacedoc.data.description/path-id #{"description"})
(s/def :spacedoc.data.description/level #{1})
(s/def :spacedoc.data.description/children :spacedoc.data.headline/children)
(defnode ::description
  (s/keys :req-un [:spacedoc.data.description/tag
                   :spacedoc.data.description/value
                   :spacedoc.data.description/path-id
                   :spacedoc.data.description/level
                   :spacedoc.data.description/children]))


;; todo node

(s/def :spacedoc.data.todo/tag #{:todo})
(s/def :spacedoc.data.todo/value ::non-empty-string)
(s/def :spacedoc.data.todo/path-id path-id?)
(s/def :spacedoc.data.todo/level (set (range 1 (inc data/max-headline-depth))))
(s/def :spacedoc.data.todo/children (s/coll-of ::headline-child
                                               :kind vector?
                                               :into []))
(defnode ::todo
  (s/keys :req-un [:spacedoc.data.todo/tag
                   :spacedoc.data.todo/value]
          :opt-un [:spacedoc.data.todo/level
                   :spacedoc.data.todo/path-id
                   :spacedoc.data.todo/children]))


;; root node

(s/def :spacedoc.data.root/tag #{:root})
(defmulti ^:private  root-child :tag)
(defmethod root-child :todo [_] ::todo)
(defmethod root-child :section [_] ::section)
(defmethod root-child :headline [_] ::headline)
(defmethod root-child :description [_] ::description)
(s/def ::root-child (s/multi-spec root-child :tag))
(s/def :spacedoc.data.root/children (s/coll-of ::root-child
                                               :kind vector?
                                               :min-count 1
                                               :distinct true
                                               :into []))
(defnode ::root (s/keys :req-un [:spacedoc.data.root/tag
                                 :spacedoc.data.root/children]))

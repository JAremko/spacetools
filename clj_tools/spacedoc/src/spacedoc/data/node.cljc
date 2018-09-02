(ns ^{:doc "Shared SDN node generators.
All public function in this name-space are node constructors.
NOTE: Format specific specs are in corresponding name-spaces.
EXAMPLE: :spacedoc.data.org/toc"}
    spacedoc.data.node
  (:require [clojure.set :refer [union map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacedoc.data :as data]
            [spacedoc.util :as u]))


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


;;;; Defnode macro

(defn- alt-cons
  [tag]
  {:pre [(keyword? tag)(u/unqualified-ident? tag)]
   :post [(or (string? %) (nil? %))]}
  ({:link "`link`"
    :plain-list "`ordered-list` and `unordered-list`."
    :description "`description`"
    :headline "`headline`"
    :todo "`todo`"
    :table "`table`"}
   tag))


(defn- sdn-key-rank
  [sdn-key]
  {:pre [(keyword? sdn-key)(u/unqualified-ident? sdn-key)]
   :post [(integer? %)]}
  (or (sdn-key
       {:tag (Integer/MIN_VALUE)
        :key (inc Integer/MIN_VALUE)
        :value (+ 2 (Integer/MIN_VALUE))
        :type -2
        :path -1
        ;; Everything else here

        ;; Always last
        :children (Integer/MAX_VALUE)}
       0)))


(defmacro defnode
  "Like `s/def` but also:
  - Creates node constructor based on spec-form spec.
  - Merge SPEC-FORM with #{<TAG>} spec where TAG is unqualified K."
  [k spec-form]
  (letfn [(sort-keys [ks] (sort-by (comp sdn-key-rank u/unqualify) ks))
          (keys->un-k->q-k [ks] (zipmap (map u/unqualify ks) ks))
          (keys->syms [ks] (mapv (comp symbol name) ks))]
    (let [alt (alt-cons (u/unqualify k))
          un-k->q-k (keys->un-k->q-k (sort-keys (u/map-spec->keys k)))
          ch-spec (:children un-k->q-k)
          q-k (vals un-k->q-k)
          u-tag (u/unqualify k)
          arg-tmpl (replace {'tag u-tag} (keys->syms q-k))
          f-name (symbol (str (name k) (when alt "*")))
          tag-spec-k (keyword (str (namespace k) "." (name k)) "tag")
          doc (str (format "\"%s\" node constructor [auto-generated]."
                           (name k))
                   (some->> alt
                            (str "\nThe node has an alternative "
                                 "human friendly constructor: ")))]
      `(do
         ;; Register node
         (defmethod data/node->spec-k ~(u/unqualify k) [_#] ~k)
         ;; Define node's spec
         (s/def ~tag-spec-k #{~u-tag})
         #_ (s/def ~k  (s/merge (s/keys :req-un [~tag-spec-k]) ~spec-form))
         (s/def ~k  ~spec-form)
         ;; Constructor function's spec
         (s/fdef ~f-name
           :args (s/cat ~@(when-let [k-m (dissoc un-k->q-k :tag)]
                            (interleave (keys k-m) (vals k-m))))
           :ret ~k
           :fn (s/and
                #(= (-> % :args (conj :tag) count)
                    (-> % :ret keys count))
                #(= (-> % :args :children count)
                    (-> % :ret :children count))))
         ;; Constructor function's definition
         (defn ~f-name
           ;; Doc-string
           ~doc
           ;; Args
           ~(vec (remove #{u-tag} arg-tmpl))
           ;; pre/post conditions
           {:pre ~(mapv (fn [s-k arg] `(s/valid? ~s-k ~arg)) q-k arg-tmpl)
            :post [(s/valid? ~k ~'%)]}
           ;; Returned value
           ~(zipmap (keys un-k->q-k) arg-tmpl))))))


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

(s/def :spacedoc.data.node.kbd/tag #{:kbd})
(s/def :spacedoc.data.node.kbd/value (s/coll-of ::non-empty-string
                                                :kind vector?
                                                :min-count 1
                                                :into []))


(defnode ::kbd (s/keys :req-un [:spacedoc.data.node.kbd/tag
                                :spacedoc.data.node.kbd/value]))


;; line-break node

(s/def :spacedoc.data.node.line-break/tag #{:line-break})
(defnode ::line-break (s/keys :req-un [:spacedoc.data.node.line-break/tag]))


;; text node

(s/def :spacedoc.data.node.text/tag #{:text})
(s/def :spacedoc.data.node.text/value string?)
(defnode ::text (s/keys :req-un [:spacedoc.data.node.text/tag
                                 :spacedoc.data.node.text/value]))


;; verbatim node

(s/def :spacedoc.data.node.verbatim/tag #{:verbatim})
(s/def :spacedoc.data.node.verbatim/value ::has-non-empty-line)
(defnode ::verbatim (s/keys :req-un [:spacedoc.data.node.verbatim/tag
                                     :spacedoc.data.node.verbatim/value]))


;; inline element

(s/def ::inline-element (s/or :inline-container ::inline-container
                              :inline-leaf ::inline-leaf))


;; inline-container children

(s/def ::inline-container-children (s/coll-of ::inline-element
                                              :kind vector?
                                              :min-count 1
                                              :into []))


;; bold node

(s/def :spacedoc.data.node.bold/tag #{:bold})
(s/def :spacedoc.data.node.bold/children ::inline-container-children)
(defnode ::bold (s/keys :req-un [:spacedoc.data.node.bold/tag
                                 :spacedoc.data.node.bold/children]))


;; italic node

(s/def :spacedoc.data.node.italic/tag #{:italic})
(s/def :spacedoc.data.node.italic/children ::inline-container-children)
(defnode ::italic (s/keys :req-un [:spacedoc.data.node.italic/tag
                                   :spacedoc.data.node.italic/children]))


;; strike-through node

(s/def :spacedoc.data.node.strike-through/tag #{:strike-through})
(s/def :spacedoc.data.node.strike-through/children ::inline-container-children)
(defnode ::strike-through
  (s/keys :req-un [:spacedoc.data.node.strike-through/tag
                   :spacedoc.data.node.strike-through/children]))


;; subscript node

(s/def :spacedoc.data.node.subscript/tag #{:subscript})
(s/def :spacedoc.data.node.subscript/children ::inline-container-children)
(defnode ::subscript (s/keys :req-un [:spacedoc.data.node.subscript/tag
                                      :spacedoc.data.node.subscript/children]))


;; superscript node

(s/def :spacedoc.data.node.superscript/tag #{:superscript})
(s/def :spacedoc.data.node.superscript/children ::inline-container-children)
(defnode ::superscript
  (s/keys :req-un [:spacedoc.data.node.superscript/tag
                   :spacedoc.data.node.superscript/children]))


;; underline node

(s/def :spacedoc.data.node.underline/tag #{:underline})
(s/def :spacedoc.data.node.underline/children ::inline-container-children)
(defnode ::underline (s/keys :req-un [:spacedoc.data.node.underline/tag
                                      :spacedoc.data.node.underline/children]))


;; link

(s/def :spacedoc.data.node.link/tag #{:link})
(s/def :spacedoc.data.node.link/path ::non-empty-string)
(s/def :spacedoc.data.node.link/type (set (keys data/link-type->prefix)))
(s/def :spacedoc.data.node.link/raw-link ::non-empty-string)
(s/def :spacedoc.data.node.link/children (s/coll-of ::inline-element
                                                    :kind vector?
                                                    :into []))
(defnode ::link (s/keys :req-un [:spacedoc.data.node.link/tag
                                 :spacedoc.data.node.link/path
                                 :spacedoc.data.node.link/type
                                 :spacedoc.data.node.link/raw-link
                                 :spacedoc.data.node.link/children]))


;; paragraph node

(s/def :spacedoc.data.node.paragraph/tag #{:paragraph})
(s/def ::paragraph-child ::inline-element)
(s/def :spacedoc.data.node.paragraph/children (s/coll-of ::paragraph-child
                                                         :kind vector?
                                                         :min-count 1
                                                         :into []))
(defnode ::paragraph (s/keys :req-un [:spacedoc.data.node.paragraph/tag
                                      :spacedoc.data.node.paragraph/children]))


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

(s/def :spacedoc.data.node.center/tag #{:center})
(s/def ::center-child ::inline-element)
(s/def :spacedoc.data.node.center/children (s/coll-of ::center-child
                                                      :kind vector?
                                                      :min-count 1
                                                      :into []))
(defnode ::center (s/keys :req-un [:spacedoc.data.node.center/tag
                                   :spacedoc.data.node.center/children]))


;; example node

(s/def :spacedoc.data.node.example/tag #{:example})
(s/def :spacedoc.data.node.example/value ::has-non-empty-line)
(defnode ::example (s/keys :req-un [:spacedoc.data.node.example/tag
                                    :spacedoc.data.node.example/value]))


;; item-children

(s/def :spacedoc.data.node.item-children/tag #{:item-children})
(s/def :spacedoc.data.node.item-children/child
  (s/or :block-element ::block-element
        :inline-element ::inline-element))
(s/def :spacedoc.data.node.item-children/children
  (s/coll-of :spacedoc.data.node.item-children/child
             :kind vector?
             :min-count 1
             :into []))
(defnode ::item-children
  (s/keys :req-un [:spacedoc.data.node.item-children/tag
                   :spacedoc.data.node.item-children/children]))


;; item-tag

(s/def :spacedoc.data.node.item-tag/tag #{:item-tag})
(s/def :spacedoc.data.node.item-tag/child ::inline-element)
(s/def :spacedoc.data.node.item-tag/children
  (s/coll-of :spacedoc.data.node.item-tag/child
             :kind vector?
             :min-count 1
             :into []))
(defnode ::item-tag (s/keys :req-un [:spacedoc.data.node.item-tag/tag
                                     :spacedoc.data.node.item-tag/children]))


;; list-item

(s/def :spacedoc.data.node.list-item/tag #{:list-item})
(s/def :spacedoc.data.node.list-item/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.node.list-item/bullet ::non-empty-string)
(s/def :spacedoc.data.node.list-item/checkbox (s/nilable #{:trans :off :on}))
(s/def :spacedoc.data.node.list-item/children (s/cat :children ::item-children
                                                     :tag (s/? ::item-tag)))
(defnode ::list-item (s/keys :req-un [:spacedoc.data.node.list-item/tag
                                      :spacedoc.data.node.list-item/type
                                      :spacedoc.data.node.list-item/bullet
                                      :spacedoc.data.node.list-item/checkbox
                                      :spacedoc.data.node.list-item/children]))


;; feature-list node

(s/def :spacedoc.data.node.feature-list/tag #{:feature-list})
(s/def :spacedoc.data.node.feature-list/type
  #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.node.feature-list/children (s/coll-of ::list-item
                                                            :kind vector?
                                                            :min-count 1
                                                            :into []))
(defnode ::feature-list
  (s/keys :req-un [:spacedoc.data.node.feature-list/tag
                   :spacedoc.data.node.feature-list/type
                   :spacedoc.data.node.feature-list/children]))


;; plain-list node

(s/def :spacedoc.data.node.plain-list/tag #{:plain-list})
(s/def :spacedoc.data.node.plain-list/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.node.plain-list/children (s/coll-of ::list-item
                                                          :kind vector?
                                                          :min-count 1
                                                          :into []))
(defnode ::plain-list
  (s/keys :req-un [:spacedoc.data.node.plain-list/tag
                   :spacedoc.data.node.plain-list/type
                   :spacedoc.data.node.plain-list/children]))


;; quote node

(s/def :spacedoc.data.node.quote/tag #{:quote})
(s/def :spacedoc.data.node.quote/children (s/coll-of ::paragraph
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::quote (s/keys :req-un [:spacedoc.data.node.quote/tag
                                  :spacedoc.data.node.quote/children]))


;; src node

(s/def :spacedoc.data.node.src/tag #{:src})
(s/def :spacedoc.data.node.src/language ::non-empty-string)
(s/def :spacedoc.data.node.src/value ::has-non-empty-line)
(defnode ::src (s/keys :req-un [:spacedoc.data.node.src/tag
                                :spacedoc.data.node.src/language
                                :spacedoc.data.node.src/value]))


;; table-row node

(s/def :spacedoc.data.node.table-row/tag #{:table-row})
(s/def :spacedoc.data.node.table-row/type #{:rule :standard})
(s/def :spacedoc.data.node.table-row/children (s/coll-of ::table-cell
                                                         :kind vector?
                                                         :min-count 0
                                                         :into []))
(defnode ::table-row (s/keys :req-un [:spacedoc.data.node.table-row/tag
                                      :spacedoc.data.node.table-row/type
                                      :spacedoc.data.node.table-row/children]))


;; table-cell node

(s/def :spacedoc.data.node.table-cell/tag #{:table-cell})
(s/def :spacedoc.data.node.table-cell/children (s/coll-of ::inline-element
                                                          :kind vector?
                                                          :min-count 0
                                                          :into []))
(defnode ::table-cell
  (s/keys :req-un [:spacedoc.data.node.table-cell/tag
                   :spacedoc.data.node.table-cell/children]))


;; table node

(s/def :spacedoc.data.node.table/tag #{:table})
(s/def :spacedoc.data.node.table/type #{:org})
(s/def :spacedoc.data.node.table/children (s/coll-of ::table-row
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::table (s/keys :req-un [:spacedoc.data.node.table/tag
                                  :spacedoc.data.node.table/type
                                  :spacedoc.data.node.table/children]))


;; verse node

(s/def :spacedoc.data.node.verse/tag #{:verse})
(s/def :spacedoc.data.node.verse/children (s/coll-of ::inline-element
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::verse (s/keys :req-un [:spacedoc.data.node.verse/tag
                                  :spacedoc.data.node.verse/children]))


;; key-word node

(s/def :spacedoc.data.node.key-word/tag #{:key-word})
(s/def :spacedoc.data.node.key-word/key ::non-empty-string)
(s/def :spacedoc.data.node.key-word/value ::non-empty-string)
(defnode ::key-word (s/keys :req-un [:spacedoc.data.node.key-word/tag
                                     :spacedoc.data.node.key-word/key
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

(s/def :spacedoc.data.node.section/tag #{:section})
(s/def :spacedoc.data.node.section/children (s/coll-of ::block-element
                                                       :kind vector?
                                                       :min-count 1
                                                       :into []))
(defnode ::section (s/keys :req-un [:spacedoc.data.node.section/tag
                                    :spacedoc.data.node.section/children]))


;;;; Headlines

(def headline-tags #{:description :todo :headline})


(defmulti ^:private headline-child :tag)
(defmethod headline-child :todo [_] ::todo)
(defmethod headline-child :section [_] ::section)
(defmethod headline-child :headline [_] ::headline)
(s/def ::headline-child (s/multi-spec headline-child :tag))


;; headline

(s/def :spacedoc.data.node.headline/tag #{:headline})
(s/def :spacedoc.data.node.headline/value ::non-empty-string)
(s/def :spacedoc.data.node.headline/path-id data/path-id?)
(s/def :spacedoc.data.node.headline/level
  (set (range 1 (inc data/max-headline-depth))))
(s/def :spacedoc.data.node.headline/children (s/coll-of ::headline-child
                                                        :kind vector?
                                                        :min-count 1
                                                        :distinct true
                                                        :into []))
(defnode ::headline
  (s/keys :req-un [:spacedoc.data.node.headline/tag
                   :spacedoc.data.node.headline/value
                   :spacedoc.data.node.headline/children]
          :opt-un [:spacedoc.data.node.headline/level
                   :spacedoc.data.node.headline/path-id]))


;; description node

(s/def :spacedoc.data.node.description/tag #{:description})
(s/def :spacedoc.data.node.description/value #{"Description"})
(s/def :spacedoc.data.node.description/path-id #{"description"})
(s/def :spacedoc.data.node.description/level #{1})
(s/def :spacedoc.data.node.description/children
  :spacedoc.data.node.headline/children)
(defnode ::description
  (s/keys :req-un [:spacedoc.data.node.description/tag
                   :spacedoc.data.node.description/value
                   :spacedoc.data.node.description/path-id
                   :spacedoc.data.node.description/level
                   :spacedoc.data.node.description/children]))


;; todo node

(s/def :spacedoc.data.node.todo/tag #{:todo})
(s/def :spacedoc.data.node.todo/value ::non-empty-string)
(s/def :spacedoc.data.node.todo/path-id data/path-id?)
(s/def :spacedoc.data.node.todo/level
  (set (range 1 (inc data/max-headline-depth))))
(s/def :spacedoc.data.node.todo/children (s/coll-of ::headline-child
                                                    :kind vector?
                                                    :into []))
(defnode ::todo
  (s/keys :req-un [:spacedoc.data.node.todo/tag
                   :spacedoc.data.node.todo/value]
          :opt-un [:spacedoc.data.node.todo/level
                   :spacedoc.data.node.todo/path-id
                   :spacedoc.data.node.todo/children]))


;; root node

(s/def :spacedoc.data.node.root/tag #{:root})
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
(defnode ::root (s/keys :req-un [:spacedoc.data.node.root/tag
                                 :spacedoc.data.node.root/children]))

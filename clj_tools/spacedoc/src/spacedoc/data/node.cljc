(ns spacedoc.data.node
  (:require [clojure.set :refer [union map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacedoc.util :as u]))


(def seps  #{\! \? \: \; \( \) \{ \} \, \. \- \\ \newline \space \tab})


(def link-type->prefix {:file "file:"
                        :http "http://"
                        :https "https://"
                        :custom-id "#"
                        :ftp "ftp://"})


(def max-headline-depth 5)


;;;; Helpers


(defmulti node->spec-k :tag)


(defn path->link-prefix
  [path]
  (->> (vals link-type->prefix)
       (filter (partial str/starts-with? path))
       (first)))


;; Headline stuff

(defn path-id?
  [val]
  (re-matches
   ;; forgive me Father for I have sinned
   #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
   val))


(defn hl-val->gh-id-base
  [hl-value]
  (str "#"
       (-> hl-value
           (str/replace " " "-")
           (str/lower-case)
           (str/replace #"[^\p{Nd}\p{L}\p{Pd}\p{Pc}]" ""))))


(defn hl-val->path-id-frag
  [hl-value]
  (-> hl-value
      (str/lower-case)
      (str/replace #"[^\p{Nd}\p{L}\p{Pd}]" " ")
      (str/trim)
      (str/replace #"\s+" "_")))


(defn fill-hl
  "Give Headline placeholder a proper tag and fill all necessary key-vals."
  ([{tag :tag value :value :as headline}]
   {:post [(s/valid? (node->spec-k %) %)]}
   (assoc headline
          :level 1
          :path-id (hl-val->path-id-frag value)))
  ([{p-level :level p-path-id :path-id :as parent-headline}
    {tag :tag value :value :as headline}]
   {:post [(s/valid? (node->spec-k %) %)]}
   (let [hl-level (inc p-level)]
     (assoc headline
            :level hl-level
            :path-id (str p-path-id "/" (hl-val->path-id-frag value))))))


;; Defnode

(defn- gen-constructor-inner
  [tag doc alt]
  {:pre [(qualified-keyword? tag)]}
  (letfn [(sort-keys [ks] (sort (fn [a _] (if (= "children" (name a)) 1 -1)) ks))
          (keys->un-k->q-k [ks] (zipmap (map u/unqualify ks) ks))
          (sym-col [col] (mapv (comp symbol name) col))]
    (let [sorted-keys (sort-keys (u/map-spec->keys tag))
          un-k->q-k (keys->un-k->q-k sorted-keys)]
      (eval
       `(defn ~(symbol (str (name tag) (when alt "*")))
          ~doc
          ~(->> sorted-keys
                (sym-col)
                (remove #{'tag})
                (replace {'children ['& 'children]})
                (flatten)
                (vec))
          {:pre ~(mapv (fn [spec-key arg] `(s/valid? ~spec-key ~arg))
                       sorted-keys
                       (replace {'tag (u/unqualify tag)
                                 'children '(vec children)}
                                (sym-col sorted-keys)))
           :post [(s/valid? ~tag ~'%)]}
          ~(zipmap (keys un-k->q-k)
                   (replace {'tag (u/unqualify tag) 'children '(vec children)}
                            (sym-col (vals un-k->q-k)))))))))


(defn gen-constructor
  [qualified-node-tag doc]
  {:pre [(qualified-keyword? qualified-node-tag)]}
  (gen-constructor-inner qualified-node-tag doc false))


(defn gen-constructor*
  [qualified-node-tag doc]
  {:pre [(qualified-keyword? qualified-node-tag)]}
  (gen-constructor-inner qualified-node-tag doc true))


(defn all-tags
  []
  (set (remove #{:default} (keys (methods node->spec-k)))))


(defn- known-node?
  [tag]
  ((all-tags) tag))


(s/def ::known-node known-node?)
(defmethod node->spec-k :default [_] ::known-node)


(s/def ::node (s/multi-spec node->spec-k :tag))


(defn- alt-cons
  [tag]
  ({:link "`link`"
    :plain-list "`ordered-list` and `unordered-list`."
    :description "`description`"
    :headline "`headline`"
    :todo "`todo`"
    :table "`table`"}
   tag))


(defmacro defnode
  "Like `s/def` but also creates node constructor based on spec-form spec."
  [k spec-form]
  `(let [alt# ~(alt-cons (u/unqualify k))]
     (defmethod node->spec-k ~(u/unqualify k) [_#] ~k)
     (s/def ~k  ~spec-form)
     (gen-constructor-inner
      ~k
      (str (format "\"%s\" node constructor [auto-generated]." ~(name k))
           (some->> alt# (str "\nThe node has an alternative constructor: ")))
      alt#)))


;;;; Constructors


;; Document constructors

(defn conj-toc
  [body-node])


;; SDN node constructors

(defn unordered-list
  "Unordered \"plain-list\" node constructor."
  [items])


(defn ordered-list
  "ordered \"plain-list\" node constructor."
  [items])


(defn link
  "\"link\" node constructor."
  [path & children]
  {:pre  [(path->link-prefix path)]}
  (let [link-prefix (path->link-prefix path)
        link-type ((map-invert link-type->prefix) link-prefix)]
    {:tag :link
     :path (str/replace-first path link-prefix "")
     :type link-type
     :raw-link path
     :children (vec children)}))


;;; Nodes definitions


;; anything

(s/def ::any any?)


;; Shared specs

(s/def ::non-empty-string (s/and string? #(>= (count  %) 1)))


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
(s/def :spacedoc.data.verbatim/value ::non-empty-string)
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
(s/def :spacedoc.data.link/type (set (keys link-type->prefix)))
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
(s/def :spacedoc.data.example/value ::non-empty-string)
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
(s/def :spacedoc.data.src/value ::non-empty-string)
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
  (set (range 1 (inc max-headline-depth))))
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
(s/def :spacedoc.data.todo/level (s/and pos-int?
                                        #(<= % max-headline-depth)))
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

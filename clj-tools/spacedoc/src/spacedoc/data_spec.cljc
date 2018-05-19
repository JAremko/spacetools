(in-ns 'spacedoc.data)


;;;; Helpers

(defmulti node->spec-k :tag)
(s/def ::known-node #{:known-node})
(defmethod node->spec-k :default [_] ::known-node)
(s/def ::node (s/multi-spec node->spec-k :tag))

(defmacro ^:private defnode
  [k spec-form]
  `(do
     (defmethod node->spec-k ~(keyword (name k)) [_#] ~k)
     (s/def ~k ~spec-form)))


;;;; anything

(s/def ::any any?)


;;;; Shared specs

(s/def ::non-empty-string (s/and string? #(>= (count  %) 1)))


;;;; inline leaf

(defmulti ^:private  inline-leaf :tag)
(defmethod inline-leaf :kbd [_] ::kbd)
(defmethod inline-leaf :line-break [_] ::line-break)
(defmethod inline-leaf :plain-text [_] ::plain-text)
(defmethod inline-leaf :verbatim [_] ::verbatim)
(s/def ::inline-leaf (s/multi-spec inline-leaf :tag))


;;;; kbd node

(s/def :spacedoc.data.kbd/tag #{:kbd})
(s/def :spacedoc.data.kbd/value (s/coll-of ::non-empty-string
                                           :kind vector?
                                           :min-count 1
                                           :into []))
(defnode ::kbd (s/keys :req-un [:spacedoc.data.kbd/tag
                                :spacedoc.data.kbd/value]))


;;;; line-break node

(s/def :spacedoc.data.line-break/tag #{:line-break})
(defnode ::line-break (s/keys :req-un [:spacedoc.data.line-break/tag]))


;;;; plain-text node

(s/def :spacedoc.data.plain-text/tag #{:plain-text})
(s/def :spacedoc.data.plain-text/value string?)
(defnode ::plain-text (s/keys :req-un [:spacedoc.data.plain-text/tag
                                       :spacedoc.data.plain-text/value]))


;;;; verbatim node

(s/def :spacedoc.data.verbatim/tag #{:verbatim})
(s/def :spacedoc.data.verbatim/value ::non-empty-string)
(defnode ::verbatim (s/keys :req-un [:spacedoc.data.verbatim/tag
                                     :spacedoc.data.verbatim/value]))


;;;; inline element

(s/def ::inline-element (s/or :inline-container ::inline-container
                              :inline-leaf ::inline-leaf))


;;;; inline-container children

(s/def ::inline-container-children (s/coll-of ::inline-element
                                              :kind vector?
                                              :min-count 1
                                              :into []))


;;;; bold node

(s/def :spacedoc.data.bold/tag #{:bold})
(s/def :spacedoc.data.bold/children ::inline-container-children)
(defnode ::bold (s/keys :req-un [:spacedoc.data.bold/tag
                                 :spacedoc.data.bold/children]))


;;;; italic node

(s/def :spacedoc.data.italic/tag #{:italic})
(s/def :spacedoc.data.italic/children ::inline-container-children)
(defnode ::italic (s/keys :req-un [:spacedoc.data.italic/tag
                                   :spacedoc.data.italic/children]))


;;;; strike-through node

(s/def :spacedoc.data.strike-through/tag #{:strike-through})
(s/def :spacedoc.data.strike-through/children ::inline-container-children)
(defnode ::strike-through (s/keys :req-un
                                  [:spacedoc.data.strike-through/tag
                                   :spacedoc.data.strike-through/children]))


;;;; subscript node

(s/def :spacedoc.data.subscript/tag #{:subscript})
(s/def :spacedoc.data.subscript/children ::inline-container-children)
(defnode ::subscript (s/keys :req-un [:spacedoc.data.subscript/tag
                                      :spacedoc.data.subscript/children]))


;;;; superscript node

(s/def :spacedoc.data.superscript/tag #{:superscript})
(s/def :spacedoc.data.superscript/children ::inline-container-children)
(defnode ::superscript (s/keys :req-un [:spacedoc.data.superscript/tag
                                        :spacedoc.data.superscript/children]))


;;;; underline node

(s/def :spacedoc.data.underline/tag #{:underline})
(s/def :spacedoc.data.underline/children ::inline-container-children)
(defnode ::underline (s/keys :req-un [:spacedoc.data.underline/tag
                                      :spacedoc.data.underline/children]))


;;;; link

(s/def :spacedoc.data.link/tag #{:file-path
                                 :org-file-path
                                 :local-org-web-link
                                 :embeddable-file-path
                                 :web-link
                                 :embeddable-web-link
                                 :headline-link})
(s/def ::link-children (s/coll-of ::inline-element
                                  :kind vector?
                                  :into []))
(s/def :spacedoc.data.link/children ::link-children)
(defnode ::link (s/keys :req-un [:spacedoc.data.link/tag
                                 :spacedoc.data.link/children]))


;;;; org-file-path node

(s/def :spacedoc.data.org-file-path/tag #{:org-file-path})
(s/def :spacedoc.data.org-file-path/value ::non-empty-string)
(s/def :spacedoc.data.org-file-path/children ::link-children)
(defnode ::org-file-path
  (s/keys :req-un [:spacedoc.data.org-file-path/tag
                   :spacedoc.data.org-file-path/value
                   :spacedoc.data.org-file-path/children]))


;;;; local-org-web-link node

(s/def :spacedoc.data.local-org-web-link/tag #{:local-org-web-link})
(s/def :spacedoc.data.local-org-web-link/value ::non-empty-string)
(s/def :spacedoc.data.local-org-web-link/raw-link ::non-empty-string)
(s/def :spacedoc.data.local-org-web-link/target-headline-gh-id string?)
(s/def :spacedoc.data.local-org-web-link/children ::link-children)
(defnode ::local-org-web-link
  (s/keys :req-un [:spacedoc.data.local-org-web-link/tag
                   :spacedoc.data.local-org-web-link/value
                   :spacedoc.data.local-org-web-link/raw-link
                   :spacedoc.data.local-org-web-link/target-headline-gh-id
                   :spacedoc.data.local-org-web-link/children]))


;;;; embeddable-file-path node

(s/def :spacedoc.data.embeddable-file-path/tag #{:embeddable-file-path})
(s/def :spacedoc.data.embeddable-file-path/value ::non-empty-string)
(s/def :spacedoc.data.embeddable-file-path/children ::link-children)
(defnode ::embeddable-file-path
  (s/keys :req-un [:spacedoc.data.embeddable-file-path/tag
                   :spacedoc.data.embeddable-file-path/value
                   :spacedoc.data.embeddable-file-path/children]))


;;;; file-path node

(s/def :spacedoc.data.file-path/tag #{:file-path})
(s/def :spacedoc.data.file-path/value ::non-empty-string)
(s/def :spacedoc.data.file-path/children ::link-children)
(defnode ::file-path
  (s/keys :req-un [:spacedoc.data.file-path/tag
                   :spacedoc.data.file-path/value
                   :spacedoc.data.file-path/children]))


;;;; embeddable-web-link node

(s/def :spacedoc.data.embeddable-web-link/tag #{:embeddable-web-link})
(s/def :spacedoc.data.embeddable-web-link/value ::non-empty-string)
(s/def :spacedoc.data.embeddable-web-link/children ::link-children)
(defnode ::embeddable-web-link
  (s/keys :req-un [:spacedoc.data.embeddable-web-link/tag
                   :spacedoc.data.embeddable-web-link/value
                   :spacedoc.data.embeddable-web-link/children]))


;;;; web-link node

(s/def :spacedoc.data.web-link/tag #{:web-link})
(s/def :spacedoc.data.web-link/value ::non-empty-string)
(s/def :spacedoc.data.web-link/children ::link-children)
(defnode ::web-link
  (s/keys :req-un [:spacedoc.data.web-link/tag
                   :spacedoc.data.web-link/value
                   :spacedoc.data.web-link/children]))


;;;; headline-link node

(s/def :spacedoc.data.headline-link/tag #{:headline-link})
(s/def :spacedoc.data.headline-link/target-headline-gh-id ::non-empty-string)
(s/def :spacedoc.data.headline-link/children ::link-children)
(defnode ::headline-link
  (s/keys :req-un [:spacedoc.data.headline-link/tag
                   :spacedoc.data.headline-link/target-headline-gh-id
                   :spacedoc.data.headline-link/children]))


;;;; paragraph node

(s/def :spacedoc.data.paragraph/tag #{:paragraph})
(s/def ::paragraph-child ::inline-element)
(s/def :spacedoc.data.paragraph/children (s/coll-of ::paragraph-child
                                                    :kind vector?
                                                    :min-count 1
                                                    :into []))
(defnode ::paragraph (s/keys :req-un [:spacedoc.data.paragraph/tag
                                      :spacedoc.data.paragraph/children]))


;;;; inline container

(defmulti ^:private  inline-container :tag)
(defmethod inline-container :bold [_] ::bold)
(defmethod inline-container :italic [_] ::italic)
(defmethod inline-container :org-file-path [_] ::org-file-path)
(defmethod inline-container :local-org-web-link [_] ::local-org-web-link)
(defmethod inline-container :embeddable-file-path [_] ::embeddable-file-path)
(defmethod inline-container :file-path [_] ::file-path)
(defmethod inline-container :embeddable-web-link [_] ::embeddable-web-link)
(defmethod inline-container :web-link [_] ::web-link)
(defmethod inline-container :headline-link [_] ::headline-link)
(defmethod inline-container :strike-through [_] ::strike-through)
(defmethod inline-container :subscript [_] ::subscript)
(defmethod inline-container :superscript [_] ::superscript)
(defmethod inline-container :paragraph [_] ::paragraph)
(defmethod inline-container :underline [_] ::underline)
(s/def ::inline-container (s/multi-spec inline-container :tag))


;;;; center node

(s/def :spacedoc.data.center/tag #{:center})
(s/def ::center-child ::inline-element)
(s/def :spacedoc.data.center/children (s/coll-of ::center-child
                                                 :kind vector?
                                                 :min-count 1
                                                 :into []))
(defnode ::center (s/keys :req-un [:spacedoc.data.center/tag
                                   :spacedoc.data.center/children]))


;;;; example node

(s/def :spacedoc.data.example/tag #{:example})
(s/def :spacedoc.data.example/value ::non-empty-string)
(defnode ::example (s/keys :req-un [:spacedoc.data.example/tag
                                    :spacedoc.data.example/value]))


;;;; item-children

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


;;;;item-tag

(s/def :spacedoc.data.item-tag/tag #{:item-tag})
(s/def :spacedoc.data.item-tag/value
  (s/or :string ::non-empty-string
        :inline-element ::inline-element))
(defnode ::item-tag (s/keys :req-un [:spacedoc.data.item-tag/tag
                                     :spacedoc.data.item-tag/value]))


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


;;;; feature-list node

(s/def :spacedoc.data.feature-list/tag #{:feature-list})
(s/def :spacedoc.data.feature-list/type #{:ordered :unordered})
(s/def :spacedoc.data.feature-list/children (s/coll-of ::list-item
                                                       :kind vector?
                                                       :min-count 1
                                                       :into []))
(defnode ::feature-list (s/keys :req-un [:spacedoc.data.feature-list/tag
                                         :spacedoc.data.feature-list/type
                                         :spacedoc.data.feature-list/children]))


;;;; plain-list node

(s/def :spacedoc.data.plain-list/tag #{:plain-list})
(s/def :spacedoc.data.plain-list/type #{:ordered :unordered :descriptive})
(s/def :spacedoc.data.plain-list/children (s/coll-of ::list-item
                                                     :kind vector?
                                                     :min-count 1
                                                     :into []))
(defnode ::plain-list (s/keys :req-un [:spacedoc.data.plain-list/tag
                                       :spacedoc.data.plain-list/type
                                       :spacedoc.data.plain-list/children]))


;;;; quote node

(s/def :spacedoc.data.quote/tag #{:quote})
(s/def :spacedoc.data.quote/children (s/coll-of ::paragraph
                                                :kind vector?
                                                :min-count 1
                                                :into []))
(defnode ::quote (s/keys :req-un [:spacedoc.data.quote/tag
                                  :spacedoc.data.quote/children]))


;;;; src node

(s/def :spacedoc.data.src/tag #{:src})
(s/def :spacedoc.data.src/language ::non-empty-string)
(s/def :spacedoc.data.src/value ::non-empty-string)
(defnode ::src (s/keys :req-un [:spacedoc.data.src/tag
                                :spacedoc.data.src/language
                                :spacedoc.data.src/value]))


;;;; table-row node

(s/def :spacedoc.data.table-row/tag #{:table-row})
(s/def :spacedoc.data.table-row/children (s/coll-of ::table-cell
                                                    :kind vector?
                                                    :min-count 0
                                                    :into []))
(defnode ::table-row (s/keys :req-un [:spacedoc.data.table-row/tag
                                      :spacedoc.data.table-row/children]))


;;;; table-cell node

(s/def :spacedoc.data.table-cell/tag #{:table-cell})
(s/def :spacedoc.data.table-cell/children (s/coll-of ::inline-element
                                                     :kind vector?
                                                     :min-count 0
                                                     :into []))
(defnode ::table-cell (s/keys :req-un [:spacedoc.data.table-cell/tag
                                       :spacedoc.data.table-cell/children]))


;;;; table node

(s/def :spacedoc.data.table/tag #{:table})
(s/def :spacedoc.data.table/type #{:org})
(s/def :spacedoc.data.table/children (s/coll-of ::table-row
                                                :kind vector?
                                                :min-count 1
                                                :into []))
(defnode ::table (s/keys :req-un [:spacedoc.data.table/tag
                                  :spacedoc.data.table/type
                                  :spacedoc.data.table/children]))


;;;; verse node

(s/def :spacedoc.data.verse/tag #{:verse})
(s/def :spacedoc.data.verse/children (s/coll-of ::inline-element
                                                :kind vector?
                                                :min-count 1
                                                :into []))
(defnode ::verse (s/keys :req-un [:spacedoc.data.verse/tag
                                  :spacedoc.data.verse/children]))


;;;; fixed-width node

(s/def :spacedoc.data.fixed-width/tag #{:fixed-width})
(s/def :spacedoc.data.fixed-width/value ::non-empty-string)
(defnode ::fixed-width (s/keys :req-un [:spacedoc.data.fixed-width/tag
                                        :spacedoc.data.fixed-width/value]))


;;;; keyword node

(s/def :spacedoc.data.keyword/tag #{:keyword})
(s/def :spacedoc.data.keyword/key ::non-empty-string)
(s/def :spacedoc.data.keyword/value ::non-empty-string)
(defnode ::keyword (s/keys :req-un [:spacedoc.data.keyword/tag
                                    :spacedoc.data.keyword/key
                                    :spacedoc.data.keyword/value]))


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
(defmethod block-element :keyword [_] ::keyword)
(defmethod block-element :fixed-width [_] ::fixed-width)
(s/def ::block-element (s/multi-spec block-element :tag))


;;;; section node

(s/def :spacedoc.data.section/tag #{:section})
(s/def :spacedoc.data.section/children (s/coll-of ::block-element
                                                  :kind vector?
                                                  :min-count 1
                                                  :into []))
(defnode ::section (s/keys :req-un [:spacedoc.data.section/tag
                                    :spacedoc.data.section/children]))


;;;; Headlines

(s/def :spacedoc.data.headline/tag keyword?)
(s/def :spacedoc.data.headline/value ::non-empty-string)
(s/def :spacedoc.data.headline/gh-id (s/and string?
                                            #(re-matches
                                              #"^[\pL\pN\p{Pc}-]+$"
                                              %)))
(s/def :spacedoc.data.headline/path-id (s/and string?
                                              #(re-matches
                                                #"^[\pL\pN\p{Pc}/-]+$"
                                                %)))
(s/def :spacedoc.data.headline/level nat-int?)
(s/def :spacedoc.data.headline/tag keyword?)
(s/def :spacedoc.data.headline/children (s/nilable vector?))

(s/def ::headline
  (s/keys :req-un [:spacedoc.data.headline/tag
                   :spacedoc.data.headline/value
                   :spacedoc.data.headline/gh-id
                   :spacedoc.data.headline/path-id
                   :spacedoc.data.headline/level
                   :spacedoc.data.headline/children]))


;; Headline until

(defmacro ^:private gen-headlines
  [max-level]
  (list*
   'do
   `(defnode ~(keyword (str doc-ns-str "/headline-level-" (inc max-level)))
      #{"Avoid deep headline nesting."})
   (map (fn [n]
          (let [[child hl]
                (mapv #(keyword
                        (str *ns*)
                        (format %1 n))
                      ["headline-level-%s-child"
                       "headline-level-%s"])
                child-mm (symbol (format "headline-level-%s-child" n))
                [tag level children]
                (mapv #(keyword
                        (format %1 n))
                      ["spacedoc.data.headline-level-%s/tag"
                       "spacedoc.data.headline-level-%s/level"
                       "spacedoc.data.headline-level-%s/children"])
                next-hl (keyword doc-ns-str
                                 (str "headline-level-" (inc n)))]
            `(do
               (s/def ~tag #{ ~(keyword (format "headline-level-%s" n))})
               (s/def ~level #{~n})
               (defmulti ^:private  ~child-mm :tag)
               (defmethod ~child-mm
                 ~(keyword (format "headline-level-%s" (inc n)))
                 [_#] ~next-hl)
               (defmethod ~child-mm :todo [_#] ::todo)
               (defmethod ~child-mm :section [_#] ::section)
               (s/def ~child (s/multi-spec ~child-mm :tag))
               (s/def ~children (s/coll-of ~child
                                           :kind vector?
                                           :min-count 1
                                           :distinct true
                                           :into []))
               (defnode ~hl
                 (s/merge
                  ::headline
                  (s/keys :req-un [~tag
                                   ~level
                                   ~children]))))))
        (range 1 (inc max-level)))))


;;;; Generate 5 levels of headline nodes

(gen-headlines 5)


;;;; description node

(s/def :spacedoc.data.description/tag #{:description})
(s/def :spacedoc.data.description/value #{"Description"})
(s/def :spacedoc.data.description/level #{1})
(s/def :spacedoc.data.description/children (s/coll-of ::headline-level-1-child
                                                      :kind vector?
                                                      :min-count 1
                                                      :distinct true
                                                      :into []))
(defnode ::description
  (s/merge
   ::headline
   (s/keys :req-un [:spacedoc.data.description/tag
                    :spacedoc.data.description/value
                    :spacedoc.data.description/level
                    :spacedoc.data.description/children])))


;;;; todo node

(s/def :spacedoc.data.todo/tag #{:todo})
(defnode ::todo
  (s/merge
   ::headline
   (s/keys :req-un [:spacedoc.data.todo/tag])))


;;;; root node

(s/def :spacedoc.data.root/tag #{:root})
(s/def :spacedoc.data.root/file-has-description? boolean?)
(s/def :spacedoc.data.root/file-has-feature-list? boolean?)
(s/def :spacedoc.data.root/headline-path-ids (s/coll-of string?
                                                        :kind vector?
                                                        :min-count 1
                                                        :distinct true
                                                        :into []))
(s/def :spacedoc.data.root/children (s/coll-of ::body
                                               :kind vector?
                                               :count 1
                                               :distinct true
                                               :into []))
(defnode ::root (s/keys :req-un [:spacedoc.data.root/tag
                                 :spacedoc.data.root/file-has-description?
                                 :spacedoc.data.root/file-has-feature-list?
                                 :spacedoc.data.root/headline-path-ids
                                 :spacedoc.data.root/children]))


;;;; body node

(s/def :spacedoc.data.body/tag #{:body})
(defmulti ^:private  body-child :tag)
(defmethod body-child :headline-level-1 [_] ::headline-level-1)
(defmethod body-child :description [_] ::description)
(defmethod body-child :todo [_] ::todo)
(defmethod body-child :section [_] ::section)
(s/def ::body-child (s/multi-spec body-child :tag))
(s/def :spacedoc.data.body/children (s/coll-of ::body-child
                                               :kind vector?
                                               :min-count 1
                                               :distinct true
                                               :into []))
(defnode ::body (s/keys :req-un [:spacedoc.data.body/tag
                                 :spacedoc.data.body/children]))

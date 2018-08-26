(in-ns 'spacedoc.data)


;;;; Helprs

(defn- alt-cons
  [tag]
  (or (when (str/starts-with? (name tag) "headline")
        "headline")
      ({:link "`link`"
        :plain-list "`ordered-list` and `unordered-list`."
        :plain-text "You can simply use strings."
        :description "`description`"
        :headline "`headline`"
        :todo "`todo`"
        :table "`table`"}
       tag)))


(defn- fmt-children
  [children]
  (mapv #(if (string? %) {:tag :plain-text :value %} %) children))


(defn- gen-constructor-inner
  [node-tag doc alt]
  (let [n-name (name node-tag)
        n-spec (keyword doc-ns-str n-name)
        keys (:keys (parse-spec n-spec))
        prop-keys (disj keys :tag :children)
        args (mapv (comp symbol name) prop-keys)
        parent (:children keys)]
    (eval `(defn ~(symbol (str n-name (when alt "*")))
             ~doc
             ~(if parent (conj args '& 'children) args)
             {:post [(s/valid? ~n-spec ~'%)]}
             ~(merge (zipmap (list* :tag prop-keys) (list* node-tag args))
                     (when parent {:children '(fmt-children children)}))))))


(defn gen-constructor
  [node-tag doc]
  (gen-constructor-inner node-tag doc false))


(defn gen-constructor*
  [node-tag doc]
  (gen-constructor-inner node-tag doc true))


(defmulti node->spec-k :tag)


(defn all-tags
  []
  (set (remove #{:default} (keys (methods node->spec-k)))))


(defn- known-node?
  [tag]
  ((all-tags) tag))


(s/def ::known-node known-node?)
(defmethod node->spec-k :default [_] ::known-node)


(s/def ::node (s/multi-spec node->spec-k :tag))


(defmacro defnode
  "Like `s/def` but also creates node constructor bases on spec-form spec."
  [k spec-form]
  (let [n-tag (keyword (name k))
        alt (alt-cons n-tag)]
    `(do
       (defmethod node->spec-k :tag [_#] ~k)
       (s/def ~k  ~spec-form)
       (gen-constructor-inner
        ~n-tag
        (str (format "\"%s\" node constructor [auto-generated]." ~(name n-tag))
             (some->> ~alt (str "\nThe node has an alternative constructor: ")))
        ~alt))))


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

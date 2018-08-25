(ns spacedoc.data.node
  (:require [spacedoc.data :as data]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [map-invert]]
            [spec-tools.parse :refer [parse-spec]]
            [clojure.string :as str]))


;;;; Document constructors

(defn conj-toc
  [body-node])


;;;; SDN node constructors

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


(defn text
  [value]
  {:tag :plain-text :value value})


;;;; Generate rest of the node constructors

(defn- alt-cons
  [tag]
  (or (when (str/starts-with? (name tag) "headline")
        "headline")
      ({:link "`link`"
        :plain-list "`ordered-list` and `unordered-list`."
        :plain-text "You can simply use strings."
        :description "`description`"
        :table "`table`"}
       tag)))


(defn- fmt-children
  [children]
  (mapv #(if (string? %) {:tag :plain-text :value %} %) children))


(defn- gen-constructor-inner
  [node-tag doc alt]
  (let [n-name (name node-tag)
        n-spec (keyword data/doc-ns-str n-name)
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


(doall
 (for [node-tag (data/all-tags)]
   (let [alt (alt-cons node-tag)]
     (gen-constructor-inner
      node-tag
      (str (format "\"%s\" node constructor [auto-generated]." (name node-tag))
           (some->> alt (str "\nThe node has an alternative constructor: ")))
      alt))))

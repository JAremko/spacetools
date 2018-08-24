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

(defn- has-alternative?
  [tag]
  (not (or (str/starts-with? (name tag) "headline")
           (#{:link
              :plain-list
              :plain-text
              :item-tag
              :list-item
              :feature-list
              :todo
              :description
              :table
              :table-row
              :table-cell
              :item-children} tag))))


(defn- fmt-children
  [children]
  (mapv #(if (string? %) ({:tag :plain-text :value %}) %) children))


(doall
 (for [n-tag data/all-tags]
   (let [n-name (name n-tag)
         n-spec (keyword data/doc-ns-str n-name)
         keys (:keys (parse-spec n-spec))
         prop-keys (disj keys :tag :children)
         args (mapv (comp symbol name) prop-keys)
         has-children (:children keys)]
     (eval `(defn ~(symbol (str n-name (when (has-alternative? n-tag) "*")))
              ~(format "\"%s\" node constructor [auto-generated]." n-name)
              ~(if has-children (conj args '& 'children) args)
              {:post [(s/valid? ~n-spec ~'%)]}
              ~(merge (when has-children {:children '(fmt-children  children)})
                      (zipmap (list* :tag prop-keys) (list* n-tag args))))))))

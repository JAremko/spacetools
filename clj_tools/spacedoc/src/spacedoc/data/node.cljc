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

(defn- node-tag->should-gen?
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


(doall
 (for [node-tag data/all-tags]
   (when (node-tag->should-gen? node-tag)
     (let [node-name (name node-tag)
           node-spec (keyword data/doc-ns-str node-name)
           keys (:keys (parse-spec node-spec))
           specific-keys (disj keys :tag :children)
           args (mapv (comp symbol name) specific-keys)
           parent (:children keys)]
       (eval `(defn ~(symbol node-name)
                ~(format "\"%s\" node SDN constructor." node-name)
                ~(if parent (conj args '& 'children) args)
                {:post [(s/valid? ~node-spec ~'%)]}
                ~(let [ret (zipmap (list* :tag specific-keys)
                                   (list* node-tag args))]
                   (if parent
                     (assoc ret :children '(vec children))
                     ret))))))))

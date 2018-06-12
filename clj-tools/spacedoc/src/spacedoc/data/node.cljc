(ns spacedoc.data.node
  (:require [spacedoc.data :as data]
            [clojure.spec.alpha :as s]
            [spec-tools.parse :refer [parse-spec]]))


;;;; SDN node constructors

(defn unordered-list
  "Unordered \"plain-list\" node constructor."
  [items])


(defn ordered-list
  "ordered \"plain-list\" node constructor."
  [items])


(defn link
  "ordered \"plain-list\" node constructor."
  [items])


;;;; Generate rest of the node constructors

(doall
 (for [node-tag data/all-tags]
   (let [node-name (name node-tag)
         node-spec (keyword data/doc-ns-str node-name)
         keys (:keys (parse-spec node-spec))
         specific-keys (->> (disj keys :tag)
                            (vec)
                            (sort-by #(= % :children)))
         args (mapv (comp symbol name) specific-keys)]
     (eval `(defn ~(symbol node-name)
              ~(format "\"%s\" node SDN constructor." node-name)
              ~args
              {:post [(s/valid? ~node-spec ~'%)]}
              ~(zipmap (list* :tag specific-keys) (list* node-tag args)))))))

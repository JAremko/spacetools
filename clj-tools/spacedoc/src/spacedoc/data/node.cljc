(ns spacedoc.data.node
  (:require [spacedoc.data :refer [doc-ns-str all-tags node->spec-k]]
            [clojure.spec.alpha :as s]
            [spec-tools.parse :refer [parse-spec]]))


;;;; SDN node constructors

(defn new-root
  "Create new \"root\" node and auto-fill its fields."
  [body-children]
  {:post [(s/valid? :spacedoc.data/root %)]}
  {:tag :root
   :file-has-description? nil
   :file-has-feature-list? nil
   :headline-path-ids nil
   :children [{:tag :body :children body-children}]})


(defn unordered-list
  "Unordered \"plain-list\" node constructor."
  [items])


(defn ordered-list
  "ordered \"plain-list\" node constructor."
  [items])


;;;; Generate rest of the node constructors

(doall
 (for [node-tag all-tags]
   (let [node-name (name node-tag)
         node-spec (keyword doc-ns-str node-name)
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

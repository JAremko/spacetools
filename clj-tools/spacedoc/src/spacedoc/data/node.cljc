(ns spacedoc.data.node
  (:require [spacedoc.data :refer [doc-ns-str all-tags]]
            [spec-tools.parse :refer [parse-spec]]))


;;;; Generate SDN node constructors.

(doall
 (for [node-tag all-tags]
   (let [node-name (name node-tag)
         keys (some->> node-name
                       (keyword doc-ns-str)
                       (parse-spec)
                       (:keys))
         specific-keys (->> (disj keys :tag)
                            (vec)
                            (sort-by #(= % :children)))
         args (mapv (comp symbol name) specific-keys)]
     (eval `(defn ~(symbol node-name)
              ~(format "\"%s\" node SDN constructor." node-name)
              ~args
              ~(zipmap (list* :tag specific-keys) (list* node-tag args)))))))

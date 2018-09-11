(ns spacedoc.data.org-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [spacedoc.data :as data]
            [spacedoc.data.org :refer :all]
            [spacedoc.shared :refer [samples]]))


(defmulti invariants
  (fn [node org-str]
    (if-let [node-spec (s/get-spec (data/node->spec-k node))]
      (cond
        ((complement s/valid?) node-spec node)
        (throw (ex-info "Invalid node" (s/explain-data node-spec node)))

        ((complement string?) org-str)
        (throw (IllegalArgumentException.
                (format "org-str: \"%s\" must be a String" org-str)))
        :else (:tag node))
      (throw (IllegalArgumentException.
              (format "node: \"%s\" must be a HashMap" node))))))


(defmethod invariants :default
  [node org-str]
  (and (map? node) (string? org-str)))


(doall
 (for [v (filter (complement indirect-nodes) (data/all-tags))
       :let [node-name (name v)]]
   (eval
    `(deftest ~(symbol (str node-name "-node->org-string"))
       (testing (format (str "Any valid \"%s\" node can "
                             "be exported to org format.")
                        ~node-name)
         (let [node-spec# (s/get-spec (data/tag->spec-k ~v))
               samples# (map first (s/exercise node-spec# (samples 10)))]
           (is (every? #(invariants % (sdn->org %)) samples#))))))))

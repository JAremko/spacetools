(ns spacedoc.data.org-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [spacedoc.data :as data]
            [spacedoc.data.org :refer :all]
            [spacedoc.shared :refer [gen-mult]]))


(doall
 (for [v (filter (complement indirect-nodes) (data/all-tags))
       :let [node-name (name v)]]
   (eval
    `(deftest ~(symbol (str node-name "-node->org-string"))
       (testing (format (str "Any valid \"%s\" node can "
                             "be exported to org format.")
                        ~node-name)
         (let [node-spec# (s/get-spec (data/tag->spec-k ~v))
               samples# (map first (s/exercise node-spec# (* gen-mult 10)))]
           (is (every? string? (map sdn->org samples#)))))))))

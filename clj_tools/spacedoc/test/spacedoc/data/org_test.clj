(ns spacedoc.data.org-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [clojure.walk :refer [postwalk]]
            [spacedoc.data :as data]
            [spacedoc.data.org :refer :all]))


(doall
 (for [v (filter (complement indirect-nodes) (data/all-tags))
       :let [node-name (name v)]]
   (eval
    `(deftest ~(symbol (str node-name "-node->org-string"))
       (testing (format (str "Any valid \"%s\" node can "
                             "be exported to org format.")
                        ~node-name)
         (let [node-spec# (s/get-spec (data/tag->spec-k ~v))
               samples# (map first (s/exercise node-spec# 10))]
           (is (every? string? (map sdn->org samples#)))))))))

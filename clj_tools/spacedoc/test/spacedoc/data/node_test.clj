(ns spacedoc.data.node-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [spacedoc.data :as data]
            [spacedoc.data.node :refer :all]))


(doall
 (for [v (vals (ns-publics 'spacedoc.data.node))
       :let [f-name (:name (meta v))]
       :when (function? (deref v))]
   (eval
    `(let [f-spec# (s/get-spec ~v)]
       ;; Speced?
       (deftest ~(symbol (str f-name "-has-spec"))
         (testing (str "Node constructor function \"" '~f-name "\" speced.")
           (is (s/spec? f-spec#))))
       ;; Spec-test
       (when f-spec#
         (binding [s/*recursion-limit* 2
                   *assert* false]
           (deftest ~(symbol (str f-name "-generates-valid-node"))
             (let [fl# (->> (s/exercise-fn ~v 10)
                            (map second)
                            (keep #(s/abbrev (s/explain-data (:ret f-spec#) %)))
                            first)]
               (is (every? nil? fl#)
                   (str "Validation fails: " fl#))))))))))

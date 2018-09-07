(ns spacedoc.data.node-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [spacedoc.data :as data]
            [spacedoc.data.node :refer :all]
            [clojure.spec.gen.alpha :as gen]))


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
       (when f-spec#)
       (deftest ~(symbol (str f-name "-generates-valid-node"))
         (binding [s/*recursion-limit* 2]
           (let [ret-spec# (:ret f-spec#)
                 fail# (->> (s/exercise-fn ~v 10)
                            (filter #(->> %
                                          (second)
                                          (s/valid? ret-spec#)
                                          (false?)))
                            (first))]
             (is (nil? fail#)
                 (format (str "Function \"%s\" validation failed\n"
                              "With\n"
                              " arguments: %s\n"
                              " returned value: %s\n"
                              "Explanation:\n%s\n")
                         ~v
                         (vec (first fail#))
                         (second fail#)
                         (s/explain-str ret-spec# (second fail#)))))))))))

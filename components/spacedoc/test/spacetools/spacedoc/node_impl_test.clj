(ns spacetools.spacedoc.node-impl-test
  "Testing node constructor spec bases generator."
  (:require [clojure.set :refer [difference]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.node-impl :refer :all]
            [spacetools.test-util.interface :as tu]))


(st/instrument)


(defspec unqualify-kv-gen
  {:num-tests (tu/samples 10)}
  (testing "The function always returns valid result"
    (prop/for-all
     [q-kv gen/keyword-ns]
     (is (unqualified-keyword? (unqualify-kv q-kv))))))


(defspec sdn-key-rank-gen
  {:num-tests (tu/samples 10)}
  (testing "The function always returns valid result"
    (prop/for-all
     [kv gen/keyword]
     (is (int? (sdn-key-rank kv))))))


(deftest map-spec->keys-fn
  (testing "The function should parse map spec"
    (is (empty? (difference #{::foo ::bar ::baz}
                            (-> '(s/keys
                                  :req-un [::foo ::bar]
                                  :opt-un [::baz])
                                map-spec->keys
                                set))))))


(s/def :testing.defnode-spec-args-fn.unsup/children (s/merge))

(s/def :testing.defnode-spec-args-fn.cat/children (s/cat :any any?))

(s/def :testing.defnode-spec-args-fn.coll-of/children (s/coll-of any?))

(s/def ::merge-spec (s/merge))

(deftest defnode-spec-args-fn
  (testing "The function should return valid argument seq"
    (is (= (defnode-spec-args [:testing.defnode-spec-args-fn.cat/children])
           '(:any clojure.core/any?)))
    (is (= (defnode-spec-args [::merge-spec]) '(:merge-spec ::merge-spec)))
    (is (= (defnode-spec-args [::merge-spec
                               :testing.defnode-spec-args-fn.cat/children])
           '(:merge-spec ::merge-spec :any clojure.core/any?)))
    (is (= (defnode-spec-args [:testing.defnode-spec-args-fn.coll-of/children])
           '(:children (clojure.spec.alpha/+ clojure.core/any?)))))

  (testing "The function should throw when passed unsupported spec"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Can't generate children args with:"
         (defnode-spec-args [:testing.defnode-spec-args-fn.unsup/children])))
    (is (thrown? Exception (defnode-spec-args [:nonexistent/children])))))


(s/def :testing.defnode-impl-fn/children (s/coll-of any?))

;; Kinda sanity check. Node constructor test this function more rigorously.
(deftest defnode-impl-fn
  (testing "The function shouldn't fail with supported node spec."
    (defnode-impl ::foo false
      '(s/keys :req-un [:testing.defnode-impl-fn/children]))
    (defnode-impl ::bar true
      '(s/keys :req-un [:testing.defnode-impl-fn/children])))
  (testing (str "The function should fail if children spec doesn't exist"
                " for constructor function generation.")
    (defnode-impl ::baz false '(s/keys :req-un [:nonexistent/children]))
    (is (thrown? Exception (defnode-impl ::qux true
                             '(s/keys :req-un [:nonexistent/children]))))))

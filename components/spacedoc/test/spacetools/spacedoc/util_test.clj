(ns spacetools.spacedoc.util-test
  "All public function in `spacetools.spacedoc.node` ns are node constructors.
  So we simply select them and generate tests based on node specs."
  (:require [clojure.set :refer [difference]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.rpl.specter :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :refer :all]
            [spacetools.test-util.interface :as tu]))


(st/instrument)


(defspec non-blank-string?-gen
  {:num-tests (tu/samples 10)}
  (prop/for-all
   [s-sample gen/string]
   (is (or (and (not (non-blank-string? s-sample))
                (str/blank? s-sample))
           (and (non-blank-string? s-sample)
                (not (str/blank? s-sample)))))))


(deftest node->children-tag-s-fn
  (let [text (n/text "foo")
        section (-> text n/paragraph n/section)
        headline (n/todo "bar")
        root (n/root section headline)]
    (is (= (node->children-tag-s text) #{}))
    (is (= (node->children-tag-s section) #{:paragraph}))
    (is (= (node->children-tag-s root) #{:section :headline}))))


(deftest fmt-problem-fn
  (let [text (n/text "foo")
        text-spec-form (s/form :spacetools.spacedoc.node/text)
        problem {:foo :bar}]
    (is (str/includes? (fmt-problem text problem) ":foo"))
    (is (str/includes? (fmt-problem text problem) ":bar"))
    (is (str/includes? (fmt-problem text problem) ":text"))
    (is (str/includes? (fmt-problem text problem) (str text-spec-form)))))


(deftest explain-deepest-fn
  (let [good-node (->> "baz"
                       n/text
                       (n/paragraph (n/text "bar"))
                       n/section
                       (n/headline "foo"))
        bad-node (setval [:value] :bad-hl-val good-node)
        double-bad-node (setval [:children FIRST
                                 :children FIRST
                                 :children LAST
                                 :value]
                                :bad-last-text-val
                                bad-node)
        triple-bad-node (setval [:children FIRST
                                 :children FIRST
                                 :children FIRST
                                 :value]
                                :bad-first-text-val
                                double-bad-node)]
    (is (nil? (explain-deepest good-node)))
    (is (some? (explain-deepest {:tag :unknown-node})))
    (is (= (-> bad-node explain-deepest :clojure.spec.alpha/value :value)
           :bad-hl-val))
    (is (= (-> double-bad-node explain-deepest :clojure.spec.alpha/value :value)
           :bad-last-text-val))
    (is (= (-> triple-bad-node explain-deepest :clojure.spec.alpha/value :value)
           :bad-first-text-val))))

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
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.util :refer :all]
            [spacetools.test-util.interface :as tu]
            [spacetools.spacedoc.node :as n]))


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

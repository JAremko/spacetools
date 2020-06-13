(ns spacetools.spacedoc.org.layers-test
  "Testing generation of layers.org SDN structure."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.org.layers :refer :all]
            [spacetools.spacedoc.util :as sdu]
            [spacetools.test-util.interface :as tu]))


;; Test vals
(def test-text "test text")
(def test-children (->> test-text n/text n/paragraph n/section vector))
(def test-description-text test-text)
(def test-description (apply n/description test-children))


;; Helpers
(defn contains-string
  "Returns true if TREE EDN structure contains S string at any level."
  [s tree]
  (->> tree
       (tree-seq (some-fn map? vector?)
                 #(or (:children %) %))
       vec
       (some #{s})
       some?))


(defn make-root
  "Make simple root for testing."
  [& children]
  (apply n/root "Test root" #{} children))


(st/instrument)

(deftest root->description-fn
  (is (nil? (root->description (n/root "foo" #{} (n/todo "bar"))))
      "The function should return nil if SDN doesn't have description.")
  (is (every? #(= (:children %) test-children)
              (map (comp root->description (partial apply n/root "foo" #{}))
                   [[test-description]
                    [test-description
                     (->> "second desc"
                          n/text
                          n/paragraph
                          n/section
                          n/description)]
                    [(n/todo "bar") test-description]]))
      "Test if every extracted description has test children."))


(deftest capitalize-first-fn
  (are [s res] (= (capitalize-first s) res)
    "foo" "Foo"
    "FOO" "FOO"
    "Foo" "Foo"
    "fOO" "FOO"
    " foo" " foo"))


(deftest describe-fn
  (let [source-link "source"
        asc-source #(assoc % :source source-link)
        no-link? (partial contains-string missing-source-link-text)
        no-desc? (partial contains-string missing-description-text)]
    ;; No link no description.
    (is ((every-pred no-link? no-desc?) (describe (make-root (n/todo "foo"))))
        (str "when supplied SDN doesn't have link and description, "
             "the function's output should include their placeholders."))
    ;; No source link but has description.
    (is (every? (every-pred no-link?
                            #_  (complement no-desc?)
                            #_  (partial contains-string test-description-text))
                (map describe
                     [(make-root test-description)
                      (make-root (n/todo "foo") test-description)]))
        (str "When only source link is missing, the function's output "
             "should include source link placeholder "
             "but not description placeholder - "
             "description representation should be included instead."))
    ;; No description but has source link.
    (is ((every-pred no-desc?
                     (complement no-link?)
                     (partial contains-string source-link))
         (describe (asc-source (make-root (n/todo "foo")))))
        (str "When only description is missing, the function's output "
             "should include description placeholder "
             "but not source link placeholder - "
             "source link should be included instead."))
    ;; Both has description and source link.
    (is ((every-pred (complement no-desc?)
                     (complement no-link?)
                     (partial contains-string source-link)
                     (partial contains-string test-description-text))
         (describe (asc-source (make-root test-description))))
        (str "When both source link and description are present in the input - "
             "no placeholders should be inserted into the function's output. "
             "Instead, source link and description representation "
             "should be included."))))

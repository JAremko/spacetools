(ns spacetools.spacedoc.org.head-test
  "Testing helpers for working with headers of documents."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [spacetools.spacedoc.config :as cfg]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.org.head :refer :all]
            [spacetools.spacedoc.util :as sdu]
            [spacetools.test-util.interface :as tu]
            [spacetools.spacedoc.org.orgify :as o]
            [clojure.set :as set]
            [clojure.core.reducers :as r]))


(st/instrument)


;;;;;;;;; NOTE: DO SENITY CHEACK ON TOC HEADLINE VALUE

;; (deftest remove-inline-head-props-fn
;;   (let [prop-keys [:title :tags]
;;         root (n/root "foo" #{} (n/todo "bar"))
;;         stripped-root (reduce dissoc root prop-keys)]
;;     (is (every? (partial = :missing) (map #(% root :missing) prop-keys))
;;         "Head props should be removed")
;;     (is (= stripped-root (remove-head-props root)))))


;; (deftest remove-inline-head-props-fn
;;   (let [title (n/key-word "title" "foo")
;;         tags  (n/key-word "tags" "bar")
;;         simple-root (n/root "foo" #{} title tags)]
;;     (is (empty? (:children (remove-inline-head-props simple-root)))
;;         "Head props should be removed")))


;; (let [title (n/key-word "TITLE" "foo")
;;       tags  (n/key-word "TAGS" "bar")
;;       head (n/section title tags)
;;       simple-root (n/root "foo" #{} head (n/todo "bar"))]
;;   (remove-inline-head-props simple-root))


;; (s/valid? :spacetools.spacedoc.node/root (n/root "foo" #{}))

;;;; Test helpers
(defn mk-toc-tmpl [& children]
  (n/headline (cfg/toc-hl-val) (apply n/section children)))


;;;; Test data
(def test-toc-simple
  "TOC with simple structure. "
  [(root->toc (n/root "foo" #{} (n/todo "bar")))
   (mk-toc-tmpl (n/unordered-list [(n/link "#bar" (n/text "bar"))]))])


(def test-toc-flat
  "TOC with flat structure."
  [(root->toc (n/root "foo" #{}
                      (n/todo "foo")
                      (n/todo "bar")
                      (n/todo "baz")))
   (mk-toc-tmpl
    (n/unordered-list [(n/link "#foo" (n/text "foo"))])
    (n/unordered-list [(n/link "#bar" (n/text "bar"))])
    (n/unordered-list [(n/link "#baz" (n/text "baz"))]))])


(def test-toc-nested
  "TOC with nested structure."
  [(->> "qux"
        n/text
        n/paragraph
        n/section
        (n/headline "baz")
        (n/headline "bar")
        (n/root "foo" #{})
        root->toc)
   (mk-toc-tmpl (n/unordered-list
                 [(n/link "#bar" (n/text "bar"))
                  (n/line-break)
                  (n/unordered-list [(n/link "#baz" (n/text "baz"))])]))])

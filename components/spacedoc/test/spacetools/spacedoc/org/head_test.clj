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

;;;; Test helpers
(defn test-toc-tmpl
  "Wraps TOC children into TOC headline."
  [& children]
  (n/headline (cfg/toc-hl-val) (apply n/section children)))


(def root-meta-tags
  "Tags of root meta nodes.
  See `:spacetools.spacedoc.org.head/root-meta`."
  #{:tags :title})


;;;; Test data
(def test-toc-simple
  "TOC with simple structure. "
  [(root->toc (n/root "foo" #{} (n/todo "bar")))
   (test-toc-tmpl (n/unordered-list [(n/link "#bar" (n/text "bar"))]))])


(def test-toc-flat
  "TOC with flat structure."
  [(root->toc (n/root "foo" #{}
                      (n/todo "foo")
                      (n/todo "bar")
                      (n/todo "baz")))
   (test-toc-tmpl
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
   (test-toc-tmpl (n/unordered-list
                   [(n/link "#bar" (n/text "bar"))
                    (n/line-break)
                    (n/unordered-list [(n/link "#baz" (n/text "baz"))])]))])


(deftest remove-root-meta-fn
  (let [meta-A (n/section (n/key-word "TITLE" "foo")
                          (n/key-word "TAGS" "foo|bar|baz"))
        meta-B (n/section (n/key-word "TITLE" "bar")
                          (n/key-word "TAGS" "qux"))
        meta+ (n/section (n/key-word "TITLE" "bar")
                         (n/key-word "TAGS" "qux")
                         (n/paragraph (n/text "text")))
        root-no-meta (n/root "foo" #{} (n/todo "bar"))
        add-meta (fn [m root] (update-in root [:children] (partial into [m])))
        root-with-meta (add-meta meta-A root-no-meta)
        root-with-2x-meta (add-meta meta-B root-with-meta)]
    (is (tu/identity? remove-root-meta root-no-meta))
    (is (= root-no-meta (remove-root-meta root-with-meta)))
   #_ (is (= root-with-meta (remove-root-meta root-with-2x-meta)))))


;; (let [meta-A (n/section (n/key-word "TITLE" "foo")
;;                         (n/key-word "TAGS" "foo|bar|baz"))
;;       meta-B (n/section (n/key-word "TITLE" "bar")
;;                         (n/key-word "TAGS" "qux"))
;;       meta+ (n/section (n/key-word "TITLE" "bar")
;;                        (n/key-word "TAGS" "qux")
;;                        (n/paragraph (n/text "text")))
;;       root-no-meta (n/root "foo" #{} (n/todo "bar"))
;;       add-meta (fn [m root] (update-in root [:children] (partial into [m])))
;;       root-with-meta (add-meta meta-A root-no-meta)
;;       root-with-2x-meta (add-meta meta-B root-with-meta)]
;;   (s/explain-str :spacetools.spacedoc.org.head/with-meta meta-A)
;;   #_ root-with-meta
;;   #_ (remove-root-meta root-with-meta)
;;   #_ (s/explain-data :spacetools.spacedoc.org.head/root-with-meta root-with-meta))


;; (s/exercise :spacetools.spacedoc.org.head/with-meta 1)

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


(defn root-tags-n?
  "Returns true if X is a root tags node."
  [x]
  (s/valid? :spacetools.spacedoc.node.meta/tags x))


(defn extract-head
  "Returns [TITLE TAGS REST-OF-HEAD] of the ROOT node"
  [root]
  (let [{[{[title & rst] :children}] :children} root
        tag-or-fr (first rst)]
    (into [title] (if (root-tags-n? tag-or-fr)
                    [tag-or-fr (vec (rest rst))]
                    [nil (vec rst)]))))


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


(deftest str-tags->set-fn
  (are [string-tags set-tags] (= set-tags (str-tags->set string-tags))
    "" #{}
    "foo" #{"foo"}
    "foo|bar" #{"foo" "bar"}
    "Foo Bar" #{"Foo Bar"}
    "Foo Bar|baz qux" #{"Foo Bar" "baz qux"}
    "foo|bar|baz|qux" #{"foo" "bar" "baz" "qux"}))


(deftest set-tags->str-fn
  (are [set-tags string-tags] (= string-tags (set-tags->str set-tags))
    #{} ""
    #{"a" "b" "c"} "a|b|c"
    #{"b" "a" "c"} "a|b|c"
    #{"foo"} "foo"
    #{"foo" "bar"} "bar|foo"
    #{"Foo Bar"} "Foo Bar"
    #{"Foo Bar" "baz qux"} "Foo Bar|baz qux"
    #{"foo" "bar" "baz" "qux"} "bar|baz|foo|qux"))


(deftest remove-root-meta-fn
  (let [h+meta-a (n/section (n/key-word "TITLE" "foo")
                            (n/key-word "TAGS" "foo|bar|baz"))
        h+meta-b (n/section (n/key-word "TITLE" "bar")
                            (n/key-word "TAGS" "qux"))
        p (n/paragraph (n/text "text"))
        h+meta+p (n/section (n/key-word "TITLE" "bar")
                            (n/key-word "TAGS" "qux")
                            p)
        h+p (n/section p)
        root-no-meta (n/root "foo" #{} (n/todo "bar"))
        fadd (fn [n root] (update root :children (partial into [n])))
        root-with-meta (fadd h+meta-a root-no-meta)
        root-with-2x-meta (fadd h+meta-b root-with-meta)
        root-with-meta+p (fadd h+meta+p root-no-meta)
        root-with-p (fadd h+p root-no-meta)]
    (is (tu/identity? remove-root-meta root-no-meta))
    (is (tu/identity? remove-root-meta root-with-p))
    (is (= root-no-meta (remove-root-meta root-with-meta)))
    (is (= root-with-meta (remove-root-meta root-with-2x-meta)))
    (is (= root-with-p (remove-root-meta root-with-meta+p)))))


(deftest add-root-meta-fn
  (let [tags (tu/tags->tag->tag-descr ["a" "b" "c"])
        ph (n/todo "bar")
        h-node (n/paragraph (n/text "text"))
        head (n/section h-node)]
    (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                     (constantly tags)}
      #(are [root-node title-val tags-val rest-head]
           (let [[title tags rest] (extract-head (add-root-meta root-node))]
             (and (= title-val (:value title))
                  (= tags-val (:value tags))
                  (= rest-head rest)))
         (n/root "foo" #{} ph) "foo" nil []
         (n/root "foo" #{"a"} ph) "foo" "a" []
         (n/root "foo" #{"a" "b"} ph) "foo" "a|b" []
         (n/root "foo" #{"b" "a"} ph) "foo" "a|b" []
         (n/root "foo" #{} head) "foo" nil [h-node]
         (n/root "foo" #{"a"} head) "foo" "a" [h-node]
         (n/root "foo" #{"a"} head ph) "foo" "a" [h-node]))))

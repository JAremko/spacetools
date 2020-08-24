(ns spacetools.test-util.interface
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.interface :as sd]
            [spacetools.test-util.core :as tu]))


(defn-spec identity? boolean?
  "Returns true if (= x (f x))"
  [f fn? x any?]
  (tu/identity? f x))


(defn-spec samples pos-int?
  "Multiplies BASE-SAMPLE-COUNT by `gen-mult` and returns it as `pos-int?`."
  [base-sample-count pos-int?]
  (tu/samples base-sample-count))


(defn-spec make-f-spec-reper fn?
  "Like `default-reporter-fn` but for spec reports."
  [re-spec s/spec? f var? f-name string?]
  (tu/make-f-spec-reper re-spec f f-name))


(defn-spec create-fs tu/filesystem?
  "Create in-memory filesystem.
STRUCT is a recursive vector of files/directories.
For example: [[:path [:to [:file-a] [:file-b]]]].
OS-KW is a keyword specifying OS family: `:unix`(default), `:osx`, `:windows`."
  ([struct vector?] (tu/create-fs struct))
  ([struct vector? os-kw keyword?] (tu/create-fs struct os-kw)))


(defmacro testing-io
  [name struct & test-forms]
  `(tu/testing-io ~name ~struct ~@test-forms))


(defn-spec contains-string? boolean?
  "Returns true if TREE contains S string at any level."
  [s string? tree seqable?]
  (tu/contains-string? s tree))


(defn-spec has-n-children? boolean?
  "Returns true if SDN NODE has N number of children."
  [n nat-int? node (s/nilable sd/node?)]
  (tu/has-n-children? n node))


(defn-spec count-n? boolean?
  "Returns true if COLL collection's  `count` is N."
  [n nat-int? coll coll?]
  (tu/count-n? n coll))


(defn-spec tags->tag->tag-descr (s/map-of string? string?)
  "Create map of tags.
  See :valid-tags of `spacetools.spacedoc.config/default-config`"
  [tags (s/coll-of string?)]
  (tu/tags->tag->tag-descr tags))

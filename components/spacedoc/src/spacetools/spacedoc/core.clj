(ns spacetools.spacedoc.core
  "Mainly `defmulti`s that other parts of the component populate."
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]))


(defmulti node->spec-k
  "Given node return fully qualified spec key for it."
  :tag)


(defmethod node->spec-k :default [_] :spacetools.spacedoc.node/known-node)


(defn-spec all-tags (s/coll-of keyword? :kind set?)
  "Return all node tags."
  []
  (-> node->spec-k
      (methods)
      (dissoc :default)
      ;; NOTE: It will throw if called before any node added.
      ;; Which is good because it should never happen :D
      (keys)
      (set)))


(s/def :spacetools.spacedoc.node/known-node #((all-tags) (:tag %)))


(defn-spec node? boolean?
  "Return true if X is a node.
NOTE: Node might not be valid."
  [x any?]
  (s/valid? :spacetools.spacedoc.node/any-node x))


(defn-spec tag->spec-k qualified-keyword?
  "Given node tag return fully qualified spec key for it."
  [node-tag keyword?]
  (node->spec-k {:tag node-tag}))


(defmulti inline-leaf
  "Given inline leaf node return corresponding spec key."
  :tag)


(s/def ::set-of-keys (s/coll-of keyword? :kind set? :into #{}))


(defn-spec inline-leaf-tags ::set-of-keys
  "Return all inline leaf node tags."
  []
  (set (keys (methods inline-leaf))))


(defmulti inline-container
  "Given inline container node return corresponding spec key."
  :tag)


(defn-spec inline-container-tags ::set-of-keys
  "All inline containers tags."
  []
  (set (keys (methods inline-container))))


(defmulti block-element
  "Given block node return corresponding spec key."
  :tag)


(defn-spec block-tags ::set-of-keys
  "All block node tags."
  []
  (set (keys (methods block-element))))


(defmulti headline-child
  "Given headline child node return corresponding spec key."
  :tag)


(defmulti root-child
  "Given root node child return corresponding spec key."
  :tag)

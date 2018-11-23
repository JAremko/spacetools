(ns spacetools.spacedoc.core
  "SDN manipulation utilities."
  (:require [clojure.set :refer [union map-invert]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]))


(defmulti node->spec-k :tag)

(defmethod node->spec-k :default [_] :spacetools.spacedoc.node/known-node)


(defn-spec all-tags (s/coll-of keyword? :kind set?)
  []
  (dissoc (methods node->spec-k) :default))


(defn-spec node? boolean?
  [node any?]
  (and (:tag node)
       (s/valid? (s/map-of keyword? any?) node)))


(defn-spec known-node? (s/nilable keyword?)
  [node node?]
  ((all-tags) (:tag node)))


(s/def :spacetools.spacedoc.node/known-node known-node?)


(defn-spec tag->spec-k qualified-keyword?
  [node-tag keyword?]
  (node->spec-k {:tag node-tag}))


(defmulti inline-leaf :tag)


(s/def ::set-of-keys (s/coll-of keyword? :kind set? :into #{}))


(defn-spec inline-leaf-tags ::set-of-keys
  []
  (set (keys (methods inline-leaf))))


(defmulti inline-container :tag)


(defn-spec inline-container-tags ::set-of-keys
  []
  (set (keys (methods inline-container))))


(defn-spec inline-tags ::set-of-keys
  []
  (set (union inline-leaf-tags inline-container-tags)))


(defmulti block-element :tag)


(defn-spec block-tags ::set-of-keys
  []
  (set (keys (methods block-element))))


(defmulti headline-child :tag)


(defmulti headlines :tag)


(defn-spec headlines-tags ::set-of-keys
  []
  (set (keys (methods headlines))))


(defmulti  root-child :tag)

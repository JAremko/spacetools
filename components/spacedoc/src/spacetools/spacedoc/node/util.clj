(ns spacetools.spacedoc.node.util
  "Helpers for writing specs and constructors for nodes."
  (:require [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]))


;;;; Headline utils

(defn-spec headline? boolean?
  "Return true if NODE is headline."
  [x any?]
  (= (:tag x) :headline))


(defn-spec headline->depth pos-int?
  "Return the depth of the most nested child headline."
  [headline headline?]
  ((fn rec [depth node]
     (if (headline? node)
       (inc (r/reduce (r/monoid max (constantly 0))
                      (r/map (partial rec depth) (:children node))))
       depth))
   0 headline))


(defn-spec clamp-headline-children headline?
  "delete child headlines of HEADLINE that are deeper than LEVEL."
  [level pos-int? headline headline?]
  ((fn rec [depth {:keys [value children todo?] :as node}]
     (assoc node :children
            (if (and (headline? node) (>= depth level))
              (->> children (remove headline?) (vec))
              (mapv (partial rec (inc depth)) children))))
   1 headline))


(defn-spec mark-empty-as-todo headline?
  "mark headline as todo if i doesn't have children."
  [headline headline?]
  (if (empty? (:children headline))
    (assoc headline :todo? true)
    headline))


(defn-spec fmt-headline headline?
  "Used to fix generated headlines so they will pass spec checks."
  [max-level pos-int? headline headline?]
  (->> headline (clamp-headline-children max-level) (mark-empty-as-todo)))


(defn-spec todo-or-has-children? boolean?
  "HEADLINE node should have children or value of `:todo?` should be `true`."
  [headline headline?]
  (or (:todo? headline) ((complement empty?) (:children headline))))

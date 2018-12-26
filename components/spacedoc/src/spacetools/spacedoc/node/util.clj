(ns spacetools.spacedoc.node.util
  "Helpers for writing specs and constructors for nodes."
  (:require [clojure.core.reducers :as r]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]))


(defn headline?
  [node]
  (= (:tag node) :headline))


(defn headline->depth
  "return how deeply children of headline go."
  [headline]
  ((fn rec [depth node]
     (if (headline? node)
       (inc (r/reduce (r/monoid max (constantly 0))
                      (r/map (partial rec depth) (:children node))))
       depth))
   0 headline))


(defn clamp-headline-children
  "delete hl headline children that are deeper than level."
  [level headline]
  ((fn rec [depth {:keys [value children todo?] :as node}]
     (assoc node :children
            (if (and (headline? node) (>= depth level))
              []
              (mapv (partial rec (inc depth)) children))))
   1 headline))


(defn mark-empty-as-todo
  "mark headline as todo if i doesn't have children."
  [headline]
  (if (empty? (:children headline))
    (assoc headline :todo? true)
    headline))


(defn fmt-headline
  "Used to fix generated headlines so they will pass spec checks."
  [max-level headline]
  (->> headline (clamp-headline-children max-level) (mark-empty-as-todo)))


(defn todo-or-has-children?
  "HEADLINE node should have children or value of `:todo?` should be `true`."
  [headline] (or (:todo? headline) (seq (:children headline))))


(defn link->link-prefix
  "given full link return corresponding prefix(usually protocol + ://)."
  [path]
  (->> (vals (cfg/link-type->prefix))
       (filter (partial str/starts-with? path))
       (first)))


(defn list-item
  "create list item node."
  [item-type idx item]
  {:tag :list-item
   :type item-type
   :bullet (if (= item-type :unordered) "- " (str (inc idx) ". "))
   :checkbox nil
   :children [{:tag :item-children :children item}]})

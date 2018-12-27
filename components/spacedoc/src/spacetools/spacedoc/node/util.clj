(ns spacetools.spacedoc.node.util
  "Helpers for writing specs and constructors for nodes."
  (:require [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]))


(def headline-gen
  "Headline spec generator."
  #(s/gen :spacetools.spacedoc.node/headline))


(defn-spec headline? boolean?
  "Return true if NODE is headline."
  [x (s/with-gen any? #(gen/one-of [(s/gen any?) (headline-gen)]))]
  (= (:tag x) :headline))


(s/def ::headline? (s/with-gen headline? headline-gen))


(defn-spec headline->depth pos-int?
  "return how deeply children of headline go."
  [headline ::headline?]
  ((fn rec [depth node]
     (if (headline? node)
       (inc (r/reduce (r/monoid max (constantly 0))
                      (r/map (partial rec depth) (:children node))))
       depth))
   0 headline))


(defn-spec clamp-headline-children ::headline?
  "delete hl headline children that are deeper than level."
  [level pos-int? headline ::headline?]
  ((fn rec [depth {:keys [value children todo?] :as node}]
     (assoc node :children
            (if (and (headline? node) (>= depth level))
              []
              (mapv (partial rec (inc depth)) children))))
   1 headline))


(defn-spec mark-empty-as-todo ::headline?
  "mark headline as todo if i doesn't have children."
  [headline ::headline?]
  (if (empty? (:children headline))
    (assoc headline :todo? true)
    headline))


(defn-spec fmt-headline ::headline?
  "Used to fix generated headlines so they will pass spec checks."
  [max-level pos-int? headline ::headline?]
  (->> headline (clamp-headline-children max-level) (mark-empty-as-todo)))


(defn-spec todo-or-has-children? boolean?
  "HEADLINE node should have children or value of `:todo?` should be `true`."
  [headline ::headline?] (or (:todo? headline) (seq (:children headline))))


(defn-spec link->link-prefix string?
  "given full link return corresponding prefix(usually protocol + ://)."
  [path (s/and string? (complement str/blank?))]
  (->> (vals (cfg/link-type->prefix))
       (filter (partial str/starts-with? path))
       (first)))


;; NOTE: Had to wrap output spec in `s/valid?` because we don't have
;;       the spec yet (and `defn-spec` queries it).
(defn-spec list-item #(s/valid? :spacetools.spacedoc.node/list-item %)
  "create list item node."
  [item-type #{:ordered :unordered}
   idx nat-int?
   item (s/+ :spacetools.spacedoc.node.item-children/child)]
  {:tag :list-item
   :type item-type
   :bullet (if (= item-type :unordered) "- " (str (inc idx) ". "))
   :checkbox nil
   :children [{:tag :item-children :children (vec item)}]})

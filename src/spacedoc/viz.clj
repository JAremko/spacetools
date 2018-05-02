(ns spacedoc.viz
  (:require [lacij.layouts.core :as g]
            [lacij.layouts.layout :as gl]
            [lacij.edit.graph :as ge]
            [lacij.view.graphview :as gv]
            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [spacedoc.data :as data]))


(defn- rand-color
  []
  (apply
   format
   "rgb(%s, %s, %s)"
   (repeatedly 3 (partial rand-nth (range 20 255 5)))))


(defn- add-nodes
  [graph t->c]
  (r/reduce
   (r/monoid
    (fn [g tag]
      (-> g
          (ge/add-node tag (name tag))
          (ge/add-label tag (when (tag (tag t->c)) ["⟳"]) :font-size "20")))
    (constantly graph))
   (keys t->c)))


(defn- add-edges
  [g edges]
  (reduce (fn [g [src dst]]
            (let [id (keyword (str (name src) "-" (name dst)))]
              (ge/add-edge g id src dst :style {:stroke-width "2"
                                                :stroke (rand-color)})))
          g
          edges))


(defn- edges
  [t->c]
  (eduction
   (partition-all 2)
   ;; NOTE: lacij has stack overflow problem.
   (filter (fn [[src dst]] (not= src dst)))
   (some->> t->c
            (reduce-kv
             (fn [rset tag children]
               (conj rset (map (partial vector tag) children)))
             #{})
            vec
            flatten)))


(defn draw-graph-svg
  [file tag->children]
  (when (seq tag->children)
    (io!
     (gv/export
      (-> (ge/graph :width 800 :height 600)
          (ge/add-default-node-attrs :shape :circle :r 80)
          (add-nodes tag->children)
          (add-edges (edges tag->children))
          (gl/layout :radial :radius 400)
          (ge/build))
      file
      :indent "yes"))))

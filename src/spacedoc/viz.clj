(ns spacedoc.viz
  (:require [lacij.layouts.core :as g]
            [lacij.layouts.layout :as gl]
            [lacij.edit.graph :as ge]
            [lacij.view.graphview :as gv]))


(defn add-nodes [g & nodes]
  (reduce (fn [g node]
            (ge/add-node g node (name node)))
          g
          nodes))


(defn add-edges [g & edges]
  (reduce (fn [g [src dst]]
            (let [id (keyword (str (name src) "-" (name dst)))]
              (ge/add-edge g id src dst)))
          g
          edges))


(defn gen-graph3
  []
  (-> (ge/graph :width 800 :height 600)
      (ge/add-default-node-attrs :width 25 :height 25 :shape :circle)
      (add-nodes :r :s :t :u :v :w :x :y :t1 :t2 :t3 :t4 :t5
                 :v1 :v2 :v3 :u1 :u2 :w1 :w2
                 :x1 :x2 :x3 :x4 :x5 :y1 :y2 :y3)
      (add-edges [:s :r] [:t :s] [:u :s] [:v :s]
                 [:t1 :t] [:t2 :t] [:t3 :t] [:t4 :t] [:t5 :t]
                 [:u1 :u] [:u2 :u] [:v1 :v] [:v2 :v] [:v3 :v]
                 [:w :r] [:w1 :w] [:w2 :w] [:y :w]
                 [:y3 :y] [:y2 :y] [:y1 :y]
                 [:x :r] [:x1 :x] [:x2 :x] [:x3 :x] [:x4 :x] [:x5 :x])))


((defn main []
   (let [g (-> (gen-graph3)
               (gl/layout :radial :radius 90)
               (ge/build))]
     (gv/export g "radial.svg" :indent "yes"))))

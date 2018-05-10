(ns spacedoc.viz
  (:require #_ [lacij.layouts.layout :as gl]
            #_ [lacij.edit.graph :as ge]
            [clojure.core.reducers :as r]))


(defn- rand-color
  []
  (apply format "rgb(%s, %s, %s)" (repeatedly 3 (partial rand-nth (range 20 255 5)))))


#_ (defn- add-nodes
     [g t->c]
     (r/reduce
      (r/monoid
       (fn [rg tag]
         (-> rg
             (ge/add-node tag (name tag))
             (ge/add-label tag (when (tag (tag t->c)) ["⟳"]) :font-size "20")))
       (constantly g))
      (keys t->c)))


#_ (defn- add-edges
     [g edges]
     (reduce (fn [g [src dst]]
               (let [id (keyword (str (name src) "-" (name dst)))]
                 (ge/add-edge g id src dst :style {:stroke-width "2"
                                                   :stroke (rand-color)})))
             g
             edges))


#_ (defn- edges
     [t->c]
     (eduction
      (partition-all 2)
      ;; NOTE: lacij has stack overflow problem with cycles.
      (filter (fn [[src dst]] (not= src dst)))
      (some->> t->c
               (reduce-kv
                (fn [rset tag children]
                  (conj rset (map (partial vector tag) children)))
                #{})
               vec
               flatten)))


#_ (defn build-graph
     [tag->children]
     (when (seq tag->children)
       (-> (ge/graph :width 1200 :height 800)
           (ge/add-default-node-attrs :shape :circle :r 90)
           (add-nodes tag->children)
           (add-edges (edges tag->children))
           (gl/layout :radial :radius 800)
           (ge/build))))

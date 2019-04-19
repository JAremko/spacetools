(ns spacetools.spacedoc.layers-org
  "layers.org generator."
  (:require [clojure.core.match :refer [match]]
            [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [clojure.string :as str :refer [join]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu :refer [hl? valid-hl?]]
            [clojure.set :refer [union]]
            [cats.core :as m]
            [clojure.set :as set]))


(defn-spec tree->sdn :spacetools.spacedoc.node/root
  "Create layers.org SDN with SHAPE from a seq of sdn DOCS."
  [shape :spacetools.spacedoc.config/layers-org-tree
   docs :spacetools.spacedoc.node/root]
  (mapv (partial
         (fn walk [ds node]
           (let [tag (if (map? node)
                       (ffirst node)
                       node)
                 f-ds (seq (filter #((:tags %) tag) ds))]
             (if (string? node)
               {(str "tag: " tag) (map :title f-ds)}
               {(str "tag: " tag)
                (map (partial walk f-ds) (second (first node)))})))
         docs)
        shape))


(def documents [(n/root "asm" #{"general" "lang" "imperative"} #_ (n/todo "FOO"))
                (n/root "forth" #{"general" "imperative" "lang"} #_ (n/todo "BAR"))

                (n/root "agda" #{"pure" "lang"} #_ (n/todo "BAZ"))

                (n/root "c-c++" #{"general" "lang" "multi-paradigm"} #_ (n/todo "QUX"))

                (n/root "html" #{"markup"} #_ (n/todo "QUUX"))


                (n/root "markdown" #{"markup"} #_ (n/todo "QUUXY"))

                ])

(tree->sdn (cfg/layers-org-shape) documents)


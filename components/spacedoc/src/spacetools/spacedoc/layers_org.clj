(ns spacetools.spacedoc.layers-org
  "layers.org generator."
  (:require [clojure.core.match :refer [match]]
            [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [clojure.string :as str :refer [join]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.org :refer [sdn->org]]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu :refer [hl? valid-hl?]]
            [clojure.set :refer [union]]
            [cats.core :as m]
            [clojure.set :as set]))


(defn-spec tree->sdn (s/nilable :spacetools.spacedoc.node/root)
  "Create layers.org SDN with SHAPE from a seq of sdn DOCS."
  [shape :spacetools.spacedoc.config/layers-org-tree
   docs :spacetools.spacedoc.node/root]
  (when docs
    (apply n/root "Configuration layers" #{}
           (:children
            ((fn walk [ds node]
               (let [tag (if (map? node)
                           (ffirst node)
                           node)]
                 (when-let [f-ds (seq (filter #((:tags %) tag) ds))]
                   (if (string? node)
                     (mapv #(n/headline tag (describe %))
                           f-ds)
                     (->> node
                          first
                          val
                          (r/map (partial walk f-ds))
                          (r/reduce (r/monoid #(if (vector? %2)
                                                 (concat %1 %2)
                                                 (conj %1 %2))
                                              vector))
                          (apply n/headline tag))))))
             docs {"layer" shape})))))


(defn-spec describe any?
  [node any?]
  (n/section (n/paragraph (n/bold (n/text (:title node))))))

(def documents [
                (n/root "asm" #{"layer" "general" "lang" "imperative"}  (n/todo "FOO"))
                (n/root "forth" #{"layer" "general" "imperative" "lang"}  (n/todo "BAR"))

                (n/root "agda" #{"layer" "pure" "lang"}  (n/todo "BAZ"))

                (n/root "c-c++" #{"layer" "general" "lang" "multi-paradigm"}  (n/todo "QUX"))

                (n/root "html" #{"layer" "markup"}  (n/todo "QUUX"))


                (n/root "markdown" #{"layer" "markup"}  (n/todo "QUUXY"))

                ])

(tree->sdn (cfg/layers-org-shape) documents)

;; (s/explain-str :spacetools.spacedoc.node/root (tree->sdn (cfg/layers-org-shape) documents))

(spit "/tmp/test.org" (sdn->org (tree->sdn (cfg/layers-org-shape) documents)))

(ns spacetools.spacedoc.org.layers
  "layers.org generator."
  (:require [clojure.core.reducers :as r]
            [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.node.val-spec :as vs]
            [spacetools.spacedoc.org.orgify :refer [sdn->org]]
            [spacetools.spacedoc.util :as sdu]))


(defn-spec root->description (s/nilable :spacetools.spacedoc.node/headline)
  [node :spacetools.spacedoc.node/root]
  (->> node
       :children
       (filter (partial s/valid? :spacetools.spacedoc.node.meta/description))
       first))


(defn-spec describe :spacetools.spacedoc.node/headline
  [node :spacetools.spacedoc.node/root]
  (apply n/headline
         (:title node)
         (into (->> (if-let [src (:source node)]
                      (n/link src (n/text "link"))
                      (n/text "<layer link is missing>"))
                    n/paragraph
                    n/section
                    vector)
               (:children
                (sdu/flatten-headline 1 (root->description node))
                (->> "README.org of the layer misses or has invalid \"Description\"."
                     n/text
                     n/bold
                     n/paragraph
                     n/section
                     (n/headline "placeholder"))))))


(defn-spec layers-sdn (s/nilable :spacetools.spacedoc.node/root)
  "Create layers.org(in SDN format) from DOCS seq of sdn."
  [docs (s/coll-of :spacetools.spacedoc.node/root)]
  (some->> (cfg/layers-org-quary)
           seq
           (hash-map "layer")
           ((fn walk [ds node]
              (let [tag (if (map? node)
                          (ffirst node)
                          node)]
                (when-let [f-ds (seq (filter #((:tags %) tag) ds))]
                  (if (string? node)
                    (apply n/headline tag (mapv #(describe %) f-ds))
                    (->> node
                         first
                         val
                         (map (partial walk f-ds))
                         (remove nil?)
                         (apply n/headline ((cfg/valid-tags) tag)))))))
            docs)
           :children
           seq
           (apply n/root "Configuration layers" #{})))

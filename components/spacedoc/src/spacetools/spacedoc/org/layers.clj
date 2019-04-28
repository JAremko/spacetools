(ns spacetools.spacedoc.org.layers
  "layers.org generator."
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu]
            [clojure.string :as str]))


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


;; TODO Replace TAG validation with SPEC on configs read.
;; TODO Handle case when the root node ends up empty.
(defn-spec layers-sdn (s/nilable :spacetools.spacedoc.node/root)
  "Create layers.org(in SDN format) from DOCS seq of sdn."
  [docs (s/coll-of :spacetools.spacedoc.node/root)]
  (some->> (cfg/layers-org-query)
           seq
           (hash-map "layer")
           ((fn walk [ds node]
              (let [tag (if (map? node)
                          (ffirst node)
                          node)]
                (if-not ((cfg/valid-tags) tag)
                  (throw (ex-info "Query has invalid tag" {:tag tag}))
                  (when-let [f-ds (seq (filter #((:tags %) tag) ds))]
                    (apply n/headline (or ((cfg/valid-tags) tag)
                                          (->> tag
                                               (format "<\"%s\" invalid tag>")
                                               str/upper-case))
                           (if (string? node)
                             (mapv #(describe %) f-ds)
                             (->> node
                                  first
                                  val
                                  (map (partial walk f-ds))
                                  (remove nil?))))))))
            docs)
           :children
           seq
           (apply n/root "Configuration layers" #{})))

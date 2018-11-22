(ns spacetools.spacedoc.interface
  (:require [spacetools.spacedoc.org  :as sdo]
            [spacetools.spacedoc.util :as sdu]))


(defn sdn->org [node] (sdo/sdn->org node))
(defn up-tags [spaceroot src r-node] (sdu/up-tags spaceroot src r-node))
(defn explain-deepest [node] (sdu/explain-deepest node))
(defn relations [parents] (sdu/relations parents))

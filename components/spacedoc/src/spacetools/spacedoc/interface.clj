(ns spacetools.spacedoc.interface
  (:require [spacetools.spacedoc.core :as core]
            [spacetools.spacedoc.org :as org]))


(defn explain-deepest
  [node]
  (core/explain-deepest node))


(defn up-tags
  [spaceroot file root-node]
  (core/up-tags spaceroot file root-node))


(defn relations
  [parents]
  (core/relations parents))


(defn sdn->org
  [node]
  (org/sdn->org node))

(ns spacetools.spacedoc.interface
  (:require [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core  :as sdc]
            [spacetools.spacedoc.org.orgify :as sdo]
            [spacetools.spacedoc.org.layers :as sdl]
            [spacetools.spacedoc.util :as sdu]))


(def config-file-name cfg/config-file-name)

(def default-config cfg/default-config)

(defn sdn->org
  [node]
  (sdo/sdn->org node))

(defn layers-sdn
  [docs]
  (sdl/layers-sdn docs))

(defn explain-deepest
  [node]
  (sdu/explain-deepest node))

(defn relations
  [parents]
  (sdu/relations parents))

(defn valid-overrides?
  [configs]
  (cfg/valid-overrides? configs))

(defn valid-node?
  [x]
  (sdu/valid-node? x))

(defn valid-root?
  [x]
  (sdu/valid-root? x))

(defn override-configs!
  [overrides]
  (cfg/override-configs! overrides))

(defn node?
  [x]
  (sdc/node? x))

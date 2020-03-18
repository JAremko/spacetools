(ns spacetools.spacedoc.interface
  (:require [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core  :as sdc]
            [spacetools.spacedoc.org.layers :as sdl]
            [spacetools.spacedoc.org.orgify :as sdo]
            [spacetools.spacedoc.util :as sdu]))


(def config-file-name
  "File name of configurations overrides file."
  cfg/config-file-name)

(def default-config
  "Default configurations."
  cfg/default-config)

(defn-spec sdn->org string?
  "Given node return its org-mode text representation."
  [node sdc/node?]
  (sdo/sdn->org node))

(defn-spec layers-sdn :spacetools.spacedoc.org.layers/maybe-root
  "Create layers.org from a seq of documentation files."
  [docs :spacetools.spacedoc.node/root]
  (sdl/layers-sdn docs))

(defn-spec explain-deepest :spacetools.spacedoc.util/maybe-problems
  "Validate each NODE recursively.
  Nodes will be validated in `postwalk` order and only
  the first invalidation will be reported.
  If multiply children of the same node are invalid the first one
  will be reported.
  The function returns `nil` If all nodes are valid."
  [node sdc/node?]
  (sdu/explain-deepest node))

(defn-spec relations :spacetools.spacedoc.util/key->set
  "Apply `relation` to PARENTS and `union` the outputs.."
  [parents :spacetools.spacedoc.util/nodes]
  (sdu/relations parents))

(defn-spec valid-overrides? boolean?
  "Return true if CONFIGS is valid override configuration.
Same as `valid-configs?` but all elements of the CONFIGS map are optional."
  [configs any?]
  (cfg/valid-overrides? configs))

(defn-spec valid-node? boolean?
  "Return true if NODE is a valid node."
  [x any?]
  (sdu/valid-node? x))

(defn-spec valid-root? boolean?
  "Return true if NODE is a valid root node."
  [x any?]
  (sdu/valid-root? x))

(defn-spec override-configs!  :spacetools.spacedoc.config/configs
  "Apply OVERRIDES to the current  configuration atom."
  [overrides :spacetools.spacedoc.config/overriding-configs]
  (cfg/override-configs! overrides))

(defn-spec node? boolean?
  "Return true if X is a node.
NOTE: Node might not be valid."
  [x any?]
  (sdc/node? x))

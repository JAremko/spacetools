(ns spacetools.spacedoc.interface)


(defn explain-deepest [node])
(defn relations [parents])
(defn sdn->org [node])
(defn up-tags [spaceroot src r-node])
(defn override-configs! [overrides])
(defn valid-configs? [configs])
(defn valid-overrides? [configs])
(defn node? [x])

(def config-file-name)
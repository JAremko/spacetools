(ns spacetools.spacedoc-io.interface
  (:require [spacetools.spacedoc-io.core :as io]))


(defn absolute [path] (io/absolute path))
(defn rebase-path [old-base new-base path] (io/rebase-path old-base new-base path))
(defn *spit [path content] (io/*spit path content))
(defn sdn-file? [x] (io/sdn-file? x))
(defn directory? [x] (io/directory? x))
(defn *sdn-fps-in-dir [input-dir-path] (io/*sdn-fps-in-dir input-dir-path))
(defn *fp->sdn [path] (io/*fp->sdn path))
(defn try-m->output [*output] (io/try-m->output *output))
(defn *read-cfg-overrides [overrides-fp] (io/*read-cfg-overrides overrides-fp))

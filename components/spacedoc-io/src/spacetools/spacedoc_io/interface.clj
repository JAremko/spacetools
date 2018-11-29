(ns spacetools.spacedoc-io.interface
  (:require [spacetools.spacedoc-io.core :as io]))


(defn absolute [path] (io/absolute path))
(defn rebase-path [old-base new-base path] (io/rebase-path old-base new-base path))
(defn *spit [path content] (io/*spit path content))
(defn sdn-file? [path] (io/sdn-file? path))
(defn directory? [path] (io/directory? path))
(defn *sdn-fps-in-dir [input-dir-path] (io/*sdn-fps-in-dir input-dir-path))
(defn *fp->sdn [path] (io/*fp->sdn path))
(defn println-err [& msg] (apply io/println-err msg))
(defn println-ok [& msg] (apply io/println-ok msg))
(defn try-m->output [*output] (io/try-m->output *output))
(defn *slurp-cfg-overrides [overrides-fp] (io/*slurp-cfg-overrides overrides-fp))

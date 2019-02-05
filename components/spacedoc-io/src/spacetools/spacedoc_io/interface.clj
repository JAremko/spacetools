(ns spacetools.spacedoc-io.interface
  (:require [spacetools.spacedoc-io.core :as io]))


(def filesystem io/filesystem)

(defn absolute
  [path]
  (io/absolute path))

(defn rebase-path
  [old-base new-base path]
  (io/rebase-path old-base new-base path))

(defn *spit
  [path content]
  (io/*spit path content))

(defn file?
  [x]
  (io/file? x))

(defn sdn-file?
  [x]
  (io/sdn-file? x))

(defn edn-file?
  [x]
  (io/edn-file? x))

(defn directory?
  [x]
  (io/directory? x))

(defn *fp->sdn
  [path]
  (io/*fp->sdn path))

(defn try-m->output
  [*output]
  (io/try-m->output *output))

(defn *read-cfg-overrides
  [overrides-fp]
  (io/*read-cfg-overrides overrides-fp))

(defn *flatten-fps
  [ext paths]
  (io/*flatten-fps ext paths))

(defmacro exception-of?
  [pred]
  `(io/exception-of? ~pred))

(ns spacetools.fs-io.interface
  (:require [spacetools.fs-io.core :as io]))


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

(defn *slurp
  [path]
  (io/*slurp path))

(defn file?
  [x]
  (io/file? x))


(defn file-ref?
  [x]
  (io/file-ref? x))


(defn file-ref->path
  [f-ref]
  (io/file-ref->path f-ref))


(defn sdn-file?
  [x]
  (io/sdn-file? x))

(defn edn-file?
  [x]
  (io/edn-file? x))

(defn directory?
  [x]
  (io/directory? x))

(defn try-m->output
  [*output]
  (io/try-m->output *output))

(defn *flatten-fps
  [ext paths]
  (io/*flatten-fps ext paths))

(defmacro exception-of?
  [pred]
  `(io/exception-of? ~pred))

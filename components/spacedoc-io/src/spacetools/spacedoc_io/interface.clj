(ns spacetools.spacedoc-io.interface
  "Project specific File-system I/O."
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface :as fio]
            [spacetools.spacedoc-io.core :as sio]
            [spacetools.spacedoc.interface :as sd]))


(defn-spec *fp->sdn (fio/exception-of? map?)
  "Read and validate .SDN file."
  ([path fio/file-ref?]
   (sio/*fp->sdn path))
  ([root-node-spec (s/or :spec s/spec? :spec-ref qualified-ident?)
    path fio/file-ref?]
   (sio/*fp->sdn root-node-spec path)))


(defn-spec *read-cfg-overrides (fio/exception-of? map?)
  "Read and validate configuration overrides from a PATH file."
  [overrides-fp fio/file-ref?]
  (sio/*read-cfg-overrides overrides-fp))


(defn-spec re-root-sdn sd/valid-root?
  "Adjust :source and :root-dir of a SDN root."
  [root-dir fio/file-ref? path fio/file-ref? doc sd/valid-root?]
  (sio/re-root-sdn root-dir path doc))


(defn-spec re-root-relative-links sd/valid-root?
  "Adjust relative links to a new root."
  [root-dir fio/file-ref? path fio/file-ref? doc sd/valid-root?]
  (sio/re-root-relative-links root-dir path doc))


(defn-spec set-ext fio/file-ref?
  [ext sio/extension? fp fio/file-ref?]
  (sio/set-ext ext fp))

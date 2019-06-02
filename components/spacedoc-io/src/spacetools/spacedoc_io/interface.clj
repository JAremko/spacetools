(ns spacetools.spacedoc-io.interface
  (:require [spacetools.spacedoc-io.core :as sio]))


(defn *fp->sdn
  ([path]
   (sio/*fp->sdn path))
  ([root-node-spec path]
   (sio/*fp->sdn root-node-spec path)))


(defn *read-cfg-overrides
  [overrides-fp]
  (sio/*read-cfg-overrides overrides-fp))


(defn re-root-sdn
  [root-dir path doc]
  (sio/re-root-sdn root-dir path doc))


(defn re-root-relative-links
  [root-dir path doc]
  (sio/re-root-relative-links root-dir path doc))


(defn set-ext
  [ext fp]
  (sio/set-ext ext fp))

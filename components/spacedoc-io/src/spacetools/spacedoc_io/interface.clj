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

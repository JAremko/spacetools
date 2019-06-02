(ns spacetools.spacedoc-io.interface)

(defn *fp->sdn [path])
(defn *fp->sdn [root-node-spec path])
(defn *read-cfg-overrides [overrides-fp])
(defn re-root-relative-links [root-dir path doc])
(defn re-root-sdn [root-dir path doc])
(defn set-ext [ext fp])

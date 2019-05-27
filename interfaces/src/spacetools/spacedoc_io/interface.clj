(ns spacetools.spacedoc-io.interface)

(defn *fp->sdn [path])
(defn *fp->sdn [root-node-spec path])
(defn *read-cfg-overrides [overrides-fp])
(defn rebase-sdn [old-root-dir new-root-dir sdn])

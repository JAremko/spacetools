(ns spacetools.spacedoc-io.interface)

(def filesystem)
(defn *fp->sdn [path])
(defn *read-cfg-overrides [overrides-fp])
(defn *spit [path content])
(defn absolute [path])
(defn directory? [x])
(defn mkdir [path])
(defn rebase-path [old-base new-base path])
(defn sdn-file? [x])
(defn edn-file? [x])
(defn try-m->output [*output])
(defn *flatten-fps [ext paths])

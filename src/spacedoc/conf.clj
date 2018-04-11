(ns spacedoc.conf)

(def ^:dynamic *n-threads* (.availableProcessors (Runtime/getRuntime)))

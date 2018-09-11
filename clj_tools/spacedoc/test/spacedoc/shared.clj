(ns spacedoc.shared
  "Shared defs for tests"
  (:require [environ.core :refer [env]]))


(def gen-mult (let [g-m (or (when-let [g-m-str (env :gentest-multiplier)]
                              (let [g-m (read-string g-m-str)]
                                (when (pos? g-m) g-m)))
                            1)]
                (prn (format "Gentest sample's count multiplier is (%s)" g-m))
                g-m))


(defn samples
  "Multiplies BASE-SAMPLE-COUNT by `gen-mult` and returns it as `pos-int?`."
  [base-sample-count]
  (max 1 (int (* gen-mult base-sample-count))))

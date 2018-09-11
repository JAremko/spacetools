(ns spacedoc.shared
  "Shared defs for tests"
  (:require [environ.core :refer [env]]))


(def gen-mult (or (when-let [g-m-str (env :gentest-multiplier)]
                    (let [g-m (read-string g-m-str)]
                      (when (pos? g-m) g-m)))
                  1))


(def samples
  "Multiplies BASE-SAMPLE-COUNT by `gen-mult` and returns it
  as `pos-int?`."
  (memoize
   (fn [base-sample-count]
     (prn (format "Gentest sample count multiplier is (%s)" gen-mult))
     (max 1 (int (* gen-mult base-sample-count))))))

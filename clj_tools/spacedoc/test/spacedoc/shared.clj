(ns spacedoc.shared
  "Shared defs for tests"
  (:require [environ.core :refer [env]]))


(def gen-mult (or (when-let [g-m-str (env :gentest-multiplier)]
                    (let [g-m (read-string g-m-str)]
                      (when (pos? g-m) g-m)))
                  1))

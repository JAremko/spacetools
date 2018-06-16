(ns spacedoc.data.helpers
  (:require [spacedoc.util :as util]
            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.string :refer [join]]
            [clojure.spec.alpha :as s]))


(defn unwrap-if
  [pred node]
  (if (pred node)
    (:children node)
    [node]))

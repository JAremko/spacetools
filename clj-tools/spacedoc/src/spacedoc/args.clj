(ns spacedoc.args
  (:require [spacedoc.io :as sio]
            [clojure.set :refer [union]]
            [cats.core :as m]
            [clojure.core.match :refer [match]]
            [cats.monad.exception :as exc]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.spec.alpha :as s]
            [clojure.core.reducers :as r]))


(defn- flatten-fps
  [pathes]
  (r/fold
   (r/monoid (m/lift-m 2 union) (exc/wrap hash-set))
   (r/map
    #(cond
       (sio/sdn-file? %) (exc/success %)
       (sio/directory? %) (sio/sdn-fps-in-dir %)
       :else (exc/failure (ex-info "File isn't a .sdn file or a directory"
                                   {:file %})))
    pathes)))


(defn parse-input
  [input]
  (exc/try-on
   (if (not-empty input)
     (flatten-fps input)
     (exc/failure
      (ex-info
       "At least one input must be specified for this action."
       {:input input})))))


(defn parse
  "Parse ARGS with `parse-opts` using OPS.
  Returns options wrapped in exception monad."
  [args ops]
  (exc/try-on
   (let [{:keys [options summary arguments errors]} (parse-opts args ops)]
     (if errors
       (exc/failure (ex-info "Bad args:" {:errors errors}))
       (exc/success (assoc options
                           :summary summary
                           :action (first arguments)
                           :a-args (vec (rest arguments))))))))

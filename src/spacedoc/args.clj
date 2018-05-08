(ns spacedoc.args
  (:require [spacedoc.io :as sio]
            [clojure.set :refer [union]]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.spec.alpha :as s]
            [clojure.core.reducers :as r]))


(defn- flatten-fps-exc
  [pathes]
  (r/reduce
   (r/monoid (m/lift-m 2 union) (exc/wrap hash-set))
   (r/map
    #(do
       (println % (sio/sdn-file? %) (sio/directory? %))
       (cond
         (sio/sdn-file? %) (exc/success %)
         (sio/directory? %) (sio/sdn-fps-in-dir-exc %)
         :else (exc/failure (ex-info "File isn't a .sdn file or a directory"
                                     {:file %}))))
    pathes)))


(defn parse-input
  [input]
  (exc/try-on
   (if (not-empty input)
     (flatten-fps-exc input)
     (m/fmap sio/sdn-fps-in-dir-exc (sio/default-input-dir-exc)))))


(defn parse
  "Parse ARGS with `parse-opts` using OPS and returns options"
  [args ops]
  (exc/try-on
   (let [{:keys [options errors]} (parse-opts args ops)]
     (if errors
       (exc/failure (ex-info "Bad args:" {:errors errors}))
       (exc/success options)))))

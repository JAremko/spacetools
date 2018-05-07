(ns spacedoc.args
  (:require [spacedoc.io :as sio]
            [clojure.core.match :refer [match]]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.spec.alpha :as s]))


(defn-  not-sdn
  [files]
  (remove sio/sdn-file? files))


(defn input->sdn-fps-exc
  [input]
  (exc/try-on
   (if (not-empty input)
     (let [[f & r :as in] input]
       (match
        [(sio/sdn-file? f) (sio/directory? f) (empty? r) (empty? (not-sdn in))]
        [true false  true  _    ] (exc/success in)
        [false true  true  _    ] (sio/input-dir->sdn-fps-exc f)
        [_     _     false true ] (exc/success in)
        [_     _     _     false] (exc/failure
                                   (ex-info "Not .sdn or unreadable files."
                                            {:files (not-sdn in)}))
        :else (exc/failure (ex-info "Fail to interpret --input"
                                    {:input input}))))
     (m/fmap sio/input-dir->sdn-fps-exc (sio/default-input-dir-exc)))))


(defn parse
  "Parse ARGS with `parse-opts` using OPS and returns options"
  [args ops]
  (exc/try-on
   (let [{:keys [options errors]} (parse-opts args ops)
         {input :input} options]
     (if errors
       (exc/failure (ex-info "Bad args:" {:errors errors}))
       (exc/success options)))))

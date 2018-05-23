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
  [paths]
  (exc/try-on
   (r/fold
    (r/monoid (m/lift-m 2 union) (exc/wrap hash-set))
    (r/map
     #(cond
        (sio/sdn-file? %) (exc/success (hash-set %))
        (sio/directory? %) (sio/sdn-fps-in-dir %)
        :else (exc/failure (ex-info
                            "File isn't a .sdn file or a readable directory."
                            {:file %})))
     paths))))


(defn parse-input
  [input]
  (io! ;; That's because we use `println` below.
   (exc/try-on
    (if (empty? input)
      (exc/failure
       (ex-info
        "At least one input must be specified for this action."
        {:input input}))
      (let [flat-input (flatten-fps input)]
        ;;FIXME: Kinda not cool that we "sideeffecting" here.
        ;;       But then wrapping it into \"writer\" will
        ;;       increase complexity for questionable benefits.
        (println (interpose \newline (list* "Inputs:" (m/extract flat-input))))
        flat-input)))))


(defn parse
  "Parse ARGS with `parse-opts` using OPS.
  Returns options wrapped in exception monad."
  [args ops]
  (exc/try-on
   (let [{:keys [options summary arguments errors]} (parse-opts args ops)]
     (if errors
       (exc/failure (ex-info "Bad args:" {:errors errors}))
       (assoc options
              :summary summary
              :action (first arguments)
              :a-args (vec (rest arguments)))))))

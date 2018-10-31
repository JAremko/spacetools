(ns spacetools.spacedoc-cli.args
  "Application arguments parsing stuff."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.tools.cli :refer [parse-opts]]
            [spacetools.spacedoc-cli.io :as sio]))


(defn- *flatten-fps
  [paths]
  (exc/try-on
   (r/fold
    (r/monoid (m/lift-m 2 union) (exc/wrap hash-set))
    (r/map
     #(cond
        (sio/sdn-file? %) (exc/success (hash-set %))
        (sio/directory? %) (sio/*sdn-fps-in-dir %)
        :else (exc/failure (ex-info
                            "File isn't a .sdn file or a readable directory."
                            {:file %})))
     paths))))


(defn *parse-fs
  [input]
  (exc/try-on
   (cond (empty? input)
         (exc/failure
          (ex-info "At least one input must be specified for this action."
                   {:input input}))

         (string? input)
         (*parse-fs [input])

         (first (remove #(or (sio/sdn-file? %) (sio/directory? %)) input))
         (exc/failure
          (ex-info "all inputs must be .SDN files or readable directories."
                   {:input input}))

         :else
         (*flatten-fps (set input)))))


(defn *parse
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

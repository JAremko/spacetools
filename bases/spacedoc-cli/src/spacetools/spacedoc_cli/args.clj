(ns spacetools.spacedoc-cli.args
  "Application arguments parsing stuff."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]
            [clojure.tools.cli :refer [parse-opts]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc-io.interface :as sio]
            [spacetools.spacedoc.interface :as sdu]
            [clojure.spec.alpha :as s]))


(def overrides-file-name
  "File name of the configuration overrides."
  "sdn_overrides.edn")


(defn-spec *parse (sio/exception-of? (s/map-of keyword? some?))
  "Parse ARGS with using OPS options(see `parse-opts` docs).
  Returns options wrapped in exception monad."
  [args vector? ops vector?]
  (exc/try-on
   (let [{:keys [options summary arguments errors]} (parse-opts args ops)]
     (if errors
       (exc/failure (ex-info "Bad args:" {:errors errors}))
       (assoc options
              :summary summary
              :action (first arguments)
              :a-args (vec (rest arguments)))))))


(defn *parse-input-files
  "Parse INPUT-FILES sequence of .sdn files and dirs (searched for .sdn files)."
  [input-files]
  (exc/try-on
   (cond (empty? input-files)
         (exc/failure
          (ex-info "At least one input file must be specified for this action."
                   {:input-files input-files}))

         (string? input-files)
         (*parse-input-files [input-files])

         ;; Not all input files are paths of .sdn file or readable directories.
         (first (remove #(or (sio/sdn-file? %) (sio/directory? %)) input-files))
         (exc/failure
          (ex-info "all input files must be .SDN files or readable directories."
                   {:input-files input-files}))

         :else
         (sio/*flatten-fps ".sdn" (set input-files)))))


(defn *configure!
  "Configure spacedoc with overrides from `sdu/config-file-name` file."
  []
  (m/mlet
   [cfg-overrides (if (sio/edn-file? overrides-file-name)
                    (sio/*read-cfg-overrides overrides-file-name)
                    (exc/success {}))]
   (m/return (when cfg-overrides (sdu/override-configs! cfg-overrides)))))

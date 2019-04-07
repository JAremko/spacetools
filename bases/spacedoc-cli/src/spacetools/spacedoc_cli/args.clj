(ns spacetools.spacedoc-cli.args
  "Application arguments parsing stuff."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.tools.cli :refer [parse-opts]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface :as io]
            [spacetools.spacedoc-io.interface :refer [*read-cfg-overrides]]
            [spacetools.spacedoc.interface :refer [override-configs!]]))


(def overrides-file-name
  "File name of the configuration overrides."
  "sdn_overrides.edn")


(defn-spec *parse (io/exception-of? (s/map-of keyword? some?))
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


(defn-spec *parse-input-files (io/exception-of? (s/coll-of string? :kind set?))
  "Parse INPUT-FILES sequence of .sdn files and dirs (searched for .sdn files)."
  [input-files (s/coll-of string? :min-count 1)]
  (exc/try-on
   (cond (empty? input-files)
         (exc/failure
          (ex-info "At least one input file must be specified for this action."
                   {:input-files input-files}))

         (string? input-files)
         (*parse-input-files [input-files])

         ;; Not all input files are paths of .sdn file or readable directories.
         (first (remove #(or (io/sdn-file? %) (io/directory? %)) input-files))
         (exc/failure
          (ex-info "all input files must be .SDN files or readable directories."
                   {:input-files input-files}))

         :else
         (io/*flatten-fps ".sdn" (set input-files)))))


(defn-spec *configure! (io/exception-of? (s/map-of qualified-keyword? any?))
  "Configure spacedoc with overrides from CONFIG-FILE file."
  [config-file string?]
  (m/mlet
   [cfg-overrides (if (io/file? config-file)
                    (*read-cfg-overrides config-file)
                    (exc/success {}))]
   (m/return (override-configs! cfg-overrides))))

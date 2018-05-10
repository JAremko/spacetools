(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :refer [parse parse-input]]
            [spacedoc.actions :as ac]
            [spacedoc.util :as util]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join]]
            [clojure.edn :as edn])
  (:gen-class))


(defn usage [options-summary]
  (join
   \newline
   ["Spacemacs documentation tools."
    ""
    "Usage: spacedoc [OPTIONS]... ACTION"
    ""
    "Options:"
    options-summary
    ""
    "Actions:"
    "  validate           Validate Spacedoc(SDN) input files."
    "  describe  SPEC     Describe spec by keyword like :spacedoc.data/root."
    "  relations          print node relations in input Spacedoc(SDN) fs."
    "  rel-graph OUT_FILE Draw SVG of node relations in input Spacedoc(SDN) fs."
    ""]))


(def ops
  [["-i" "--input INPUT" "Input directory or Spacedoc(SDN) file. Can be reused."
    :parse-fn #(edn/read-string (str "\"" % "\""))
    :validate [(fn [in]
                 (and (string? in) (or (sio/directory? in) (sio/sdn-file? in))))
               "Input should be a directory or a .SDN file."]
    :assoc-fn (fn [m key val] (update m key (partial concat (list val))))]
   ["-h" "--help" "Show help message."]])


(defn fail
  [msg dat]
  (exc/failure (ex-info msg dat)))


(defn -main [& args]
  (let
      [output-m
       (m/alet
        [{:keys [help input summary action a-args]} (parse args ops)]
        (if help
          (exc/success (usage summary))
          (match
           ;; Handlers
           [action      a-args]
           ["describe"  [key]]
           (ac/describe-spec key)
           ["rel-graph" [path]]
           (m/fmap (partial ac/draw-relations-graph path) (parse-input input))
           ["validate"  []]
           (m/fmap ac/validate (parse-input input))
           ["relations" []]
           (m/fmap ac/relations (parse-input input))
           ;; Errors
           ["describe"  _]
           (fail "\"describe\" requires keyword as a single arg."
                 {:args a-args})
           ["rel-graph" _]
           (fail "\"rel-graph\" requires file name as a single arg."
                 {:args a-args})
           ["validate"  _]
           (fail "\"validate\" doesn't take args."
                 {:args a-args})
           ["relations" _]
           (fail "\"relations\" doesn't take args."
                 {:args a-args})
           [nil         _]
           (fail "No action specified."
                 {:action action})
           :else (ex-info "Invalid action" {:action action}))))
       output (m/extract output-m)]
    (if (exc/failure? output-m)
      (sio/exit-err (util/err->msg output))
      (sio/exit-success output))))

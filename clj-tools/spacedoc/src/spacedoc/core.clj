(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :refer [parse parse-input]]
            [spacedoc.actions :as ac]
            [spacedoc.util :as util]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join]])
  (:gen-class))


(defn usage [options-summary]
  (join
   \newline
   ["Spacemacs documentation tools for .SDN files."
    ""
    "NOTE: .SDN files are produced by \"spacedoc/emacs-tools/export/\"."
    ""
    "Usage: spacedoc ACTION [OPTIONS]... [ARGS]..."
    ""
    "Options:"
    options-summary
    ""
    "Actions:"
    "  validate  INPUTS... Validate input .SDN files."
    "  relations INPUTS... Print node relations in the input .SDN files."
    "  describe  SPEC      Describe spec by keyword(like :spacedoc.data/root)."
    ""]))


(def ops [["-h" "--help" "Show help message."]])


(defn fail
  [msg dat]
  (exc/failure (ex-info msg dat)))


(defn -main [& args]
  (let
      [output-m
       (m/alet
        [{:keys [help summary action a-args]} (parse args ops)]
        (if help
          (usage summary)
          (match
           ;; Handlers
           [action      a-args]
           ["describe"  [key    ]] (ac/describe-spec key)
           ["validate"  [_   & _]] (m/fmap ac/validate (parse-input a-args))
           ["relations" [_   & _]] (m/fmap ac/relations (parse-input a-args))
           ;; Errors
           ["describe"  _] (fail
                            "\"describe\" requires keyword as a single argument"
                            {:args a-args})
           ["validate"  _] (fail
                            "\"validate\" requires one or more input argument"
                            {:args a-args})
           ["relations" _] (fail
                            "\"relations\" requires one or more input argument"
                            {:args a-args})
           [nil         _] (fail
                            "No action specified. Run with \"--help\" for usage"
                            {:action action})
           :else (ex-info "Invalid action" {:action action}))))
       output (m/extract output-m)]
    (if (exc/failure? output-m)
      (sio/exit-err (util/err->msg output))
      (sio/exit-success output))))

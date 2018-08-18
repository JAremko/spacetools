(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :refer [*parse *parse-inputs]]
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
    "  validate  INS...        Validate input .SDN files."
    "  relations INS...        Print node relations in the input .SDN files."
    "  orgify    SOURCE TARGET TARGET Convert .SDN files into .ORG files."
    "                          SOURCE is parent directory with .SDN files."
    "                          TARGET is target directory for .ORG files."
    "  describe  SPEC          Describe spec by fully qualified keyword."
    "                          Example :spacedoc.data/<keyword>"
    ""]))


(def ops [["-h" "--help" "Show help message."]])


(defn fail
  [msg dat]
  (exc/failure (ex-info msg dat)))


(defn -main [& args]
  (let
      [output-m
       (m/alet
        [{:keys [help summary action a-args]} (*parse args ops)]
        (if help
          (usage summary)
          (match
           ;; Handlers
           [action      a-args]
           ["describe"  [key    ]] (ac/*describe-spec key)
           ["validate"  [_   & _]] (m/fmap ac/*validate (*parse-inputs a-args))
           ["relations" [_   & _]] (m/fmap ac/*relations (*parse-inputs a-args))
           ["orgify"    [s   t  ]] (m/fmap (partial ac/*orgify s t)
                                           (*parse-inputs [s]))
           ;; Errors
           ["describe"  _] (fail
                            "\"describe\" takes keyword as a single argument"
                            {:args a-args})
           ["validate"  _] (fail
                            "\"validate\" takes one or more input argument"
                            {:args a-args})
           ["orgify"    _]  (fail
                             "\"orgify\" takes two arguments"
                             {:args a-args})
           ["relations" _] (fail
                            "\"relations\" takes one or more input argument"
                            {:args a-args})
           [nil         _] (fail
                            "No action specified"
                            {:action action})
           :else (ex-info "Invalid action"
                          {:action action}))))
       output (m/extract output-m)]
    (if (exc/failure? output-m)
      (sio/exit-err (str
                     "\nError:\n"
                     (util/err->msg output)
                     "\n Run with \"--help\" for usage"))
      (sio/exit-success output))))

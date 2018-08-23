(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :refer [*parse]]
            [spacedoc.actions :as ac]
            [spacedoc.util :refer [err->msg fail]]
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


(defn -main [& args]
  (let [*output (m/mlet
                 [{:keys [help summary action a-args]} (*parse args ops)]
                 (if help
                   (usage summary)
                   (match
                    ;; Handlers
                    [action      a-args       ]
                    ["describe"  [key        ]] (ac/*describe-spec key)
                    ["validate"  [_     &   _]] (ac/*validate a-args)
                    ["relations" [_     &   _]] (ac/*relations a-args)
                    ["orgify"    [src   trg  ]] (ac/*orgify src trg src)
                    ;; Errors
                    ["describe"  _] (fail "\"describe\" takes keyword"
                                          {:args a-args})
                    ["validate"  _] (fail "\"validate\" takes at least 1 input"
                                          {:args a-args})
                    ["orgify"    _]  (fail "\"orgify\" takes 2 arguments"
                                           {:args a-args})
                    ["relations" _] (fail "\"relations\" takes at least 1 input"
                                          {:args a-args})
                    [nil         _] (fail "No action specified")
                    :else (ex-info "Invalid action" {:action action}))))
        output (m/extract *output)]
    (if (exc/failure? *output)
      (sio/exit-err (str
                     "\nError:\n"
                     (err->msg output)
                     "Run with \"--help\" for usage"))
      (sio/exit-success output))))

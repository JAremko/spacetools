(ns spacetools.spacedoc-cli.core
  "Spacemacs documentation tools for .SDN files."
  (:require [cats.core :refer [mlet]]
            [cats.monad.exception :refer [failure]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join]]
            [spacetools.spacedoc-cli.actions :as ac]
            [spacetools.spacedoc-cli.args :refer [*parse *configure!]]
            [spacetools.spacedoc-cli.io :refer [try-m->output]])
  (:gen-class))


(defn usage [options-summary]
  (join
   \newline
   ["Spacemacs documentation tools for .SDN files."
    ""
    ".SDN files are produced by \"sdnizer.el\" Emacs script."
    ""
    "Usage: spacetools.spacedoc ACTION [OPTIONS]... [ARGS]..."
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
    "                          Example :spacetools.spacedoc.node/<keyword>"
    ""]))


(def ops [["-h" "--help" "Show help message."]])


(defn bad-args-handler
  [action a-args]
  (match
   [action      a-args]
   ["describe"  _     ] (failure {} "\"describe\" takes qualified keyword")
   ["validate"  _     ] (failure {} "\"validate\" takes at least 1 input")
   ["orgify"    _     ] (failure {:args a-args} "\"orgify\" takes 2 arguments")
   ["relations" _     ] (failure {} "\"relations\" takes at least 1 input")
   [nil         _     ] (failure {} "No action specified")
   :else (failure {:action action} "Invalid action")))


(defn -main [& args]
  (try-m->output
   (mlet
    [{:keys [help summary action a-args]} (*parse args ops)
     _ (*configure!)]
    (if help
      (usage summary)
      (match
       ;; Handlers:
       [action      a-args       ]
       ["describe"  [key        ]] (ac/*describe-spec key)
       ["validate"  [_     &   _]] (ac/*validate a-args)
       ["relations" [_     &   _]] (ac/*relations a-args)
       ["orgify"    [src   trg  ]] (ac/*orgify src trg src)
       :else (bad-args-handler action a-args))))))

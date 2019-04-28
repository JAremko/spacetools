(ns spacetools.spacedoc-cli.run
  "Tools for Spacemacs documentation files in .sdn format."
  (:require [cats.core :as m]
            [cats.monad.exception :refer [failure]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join]]
            [spacetools.fs-io.interface :refer [try-m->output]]
            [spacetools.spacedoc-cli.actions :as ac]
            [spacetools.spacedoc-cli.args :refer [*parse *configure!]]
            [spacetools.spacedoc.interface :refer [config-file-name]]
            [clojure.string :as str]
            [cats.monad.exception :as exc])
  (:gen-class))


(defn usage [options-summary]
  (join
   \newline
   ["Tools for Spacemacs documentation files in .sdn format."
    ""
    ".SDN files are produced by \"sdnizer.el\" Emacs script."
    ""
    "Usage: ACTION [OPTIONS]... [ARGS]..."
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
    "  layers    DIR           Create LAYERS.sdn file in DIR using SDN"
    "                          files from the directory."
    ""]))


(def ops [["-h" "--help" "Show help message."]
          ["-c" "--config CONFIG" "Configuration file path"
           :validate [(complement str/blank?)
                      "Configuration path should be a string"]]])


(defn bad-args-handler
  [action a-args]
  (match
   [action      a-args]
   ["describe"  _     ] (failure {} "\"describe\" takes qualified keyword")
   ["validate"  _     ] (failure {} "\"validate\" takes at least 1 input")
   ["orgify"    _     ] (failure {:args a-args} "\"orgify\" takes 2 arguments")
   ["relations" _     ] (failure {} "\"relations\" takes at least 1 input")
   ["layers"    _     ] (failure {:args a-args} "\"layers\" takes 1 argument")
   [nil         _     ] (failure {} "No action specified")
   :else (failure {:action action} "Invalid action")))


(defn -main [& args]
  (try-m->output
   (m/do-let
    [{:keys [help summary action a-args config]} (*parse args ops)]
    (*configure! config)
    (if help
      (exc/success (usage summary))
      (match
       ;; Handlers:
       [action      a-args       ]
       ["describe"  [key        ]] (ac/*describe-spec key)
       ["validate"  [_     &   _]] (ac/*validate a-args)
       ["relations" [_     &   _]] (ac/*relations a-args)
       ["orgify"    [src   trg  ]] (ac/*orgify src trg)
       ["layers"    [src        ]] (ac/*layers src)
       :else (bad-args-handler action a-args))))))

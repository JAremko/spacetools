(ns spacetools.observatory-cli.elisp.keybinding
  "Keybinding extraction tools.."
  (:require [clojure.spec.alpha :as s]
            [medley.core :refer [deep-merge update-existing]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.parser :as parser]
            [spacetools.observatory-cli.elisp.ast :as ast]))


;;;; Values

(def legacy-binding-functions
  "Old functions for key bindings."
  #{"which-key-add-key-based-replacements"
   "which-key-add-major-mode-key-based-replacements"
   "spacemacs/declare-prefix"
   "spacemacs/declare-prefix-for-mode"
   "spacemacs/set-leader-keys"
   "spacemacs/set-leader-keys-for-major-mode"
   "spacemacs/set-leader-keys-for-minor-mode"})


;; (defn-spec collect-legacy-bindings ::legacy-bindings)

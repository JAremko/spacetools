(ns spacetools.observatory-cli.elisp.keybinding
  "Keybinding extraction tools.."
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.ast :as ast]
            [spacetools.observatory-cli.elisp.spec :as es]))


;;;; Values

(def legacy-binding-symbols
  "Symbols of old functions for prefix and key bindings."
  #{"which-key-add-key-based-replacements"
    "which-key-add-major-mode-key-based-replacements"
    "spacemacs/declare-prefix"
    "spacemacs/declare-prefix-for-mode"
    "spacemacs/set-leader-keys"
    "spacemacs/set-leader-keys-for-major-mode"
    "spacemacs/set-leader-keys-for-minor-mode"})


(defn-spec collect-legacy-bindings ::es/legacy-bindings
  "Crawls AST and returns positions of legacy bindings."
  [ast ::es/ast]
  (let [lbs (volatile! [])]
    (ast/walk-ast
     identity
     (fn log-lb
       [{:keys [tag value] :as node}]
       (when (and (= :symbol tag)
                  (legacy-binding-symbols value))
         (let [{{:keys [row column index]} :clj-antlr/position} (meta node)]
           (vswap! lbs conj {:fn-symbol value
                             :position {:row row :col column :idx index}}))))
     ast)
    @lbs))

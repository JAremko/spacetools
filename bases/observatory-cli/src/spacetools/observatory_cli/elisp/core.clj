(ns spacetools.observatory-cli.elisp.core
  "Elisp tools core functions."
  (:require [clojure.spec.alpha :as s]
            [medley.core :refer [deep-merge update-existing]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.parser :as parser]
            [spacetools.observatory-cli.elisp.ast :as ast]))


;;;; Core functions

(defn-spec read-str :spacetools.observatory-cli.elisp.ast/ast
  "Converts EmacsLisp string into AST representation."
  [s string?]
  (parser/walk-parse-tree identity
                          ast/parse-tree->ast-visitor
                          (parser/elisp-str->pruned-parse-tree s)))


;; (meta (second (:children (read-str "foo bar"))))

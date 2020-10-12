(ns spacetools.observatory-cli.elisp.core
  "Elisp tools core functions."
  (:require [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.ast :as ast]
            [spacetools.observatory-cli.elisp.parser :as parser]
            [spacetools.observatory-cli.elisp.spec :as es]))


;;;; Core functions

(defn-spec read-str ::es/ast
  "Converts EmacsLisp string into AST representation."
  [s string?]
  (parser/walk-parse-tree identity
                          ast/parse-tree->ast-visitor
                          (parser/elisp-str->pruned-parse-tree s)))

(ns spacetools.observatory-cli.elisp.spec
  "Specs."
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]))


;;;; Values

(def prefix-node-tags
  "All tags of prefix nodes."
  #{:template :spread :quote :hole})

(def coll-node-tags
  "All tags of collections nodes."
  #{:list :vector :root})

(def terminal-node-tags
  "All tags of terminal nodes."
  #{:symbol :keyword :char :string :number})

(def junk-node-tags
  "Junk tags."
  #{:whitespace :comment})

(def all-pt-nodes
  "All parser tree nodes."
  (set/union prefix-node-tags
             coll-node-tags
             terminal-node-tags
             junk-node-tags))

(def all-ast-nodes
  "All AST nodes."
  (set/union prefix-node-tags
             coll-node-tags
             terminal-node-tags))


;;;; Predicates

(defn-spec metable? boolean?
  "Returns true if X can hold meta."
  [x any?]
  (instance? clojure.lang.IObj x))


;;;; Specs

(s/def ::parse-tree (s/cat :tag all-pt-nodes
                           :content (s/+ (s/or :terminal string?
                                               :child ::parse-tree))))


(s/def ::pruned-parse-tree (s/cat :tag all-ast-nodes
                                  :content (s/+ (s/or :terminal string?
                                                      :child ::parse-tree))))


;; AST spec
(s/def ::ast-tag all-ast-nodes)
(s/def ::ast-prefix (s/coll-of prefix-node-tags :kind vector?))
(s/def ::ast-value (s/and string? seq))
(s/def ::ast-children (s/coll-of ::ast :kind vector?))

(s/def ::ast (s/keys :req-un [::ast-tag]
                     :opt-un [::ast-prefix ::ast-value ::ast-children]))


(s/def ::position
  (s/keys :req-un [::row nat-int? ::col nat-int? ::idx nat-int?]) )


(s/def ::fn-symbol (s/and string? seq))

(s/def ::legacy-binding (s/keys :req-un [::fn-symbol ::position]))

(s/def ::legacy-bindings (s/coll-of ::legcy-binding :kind vector?))

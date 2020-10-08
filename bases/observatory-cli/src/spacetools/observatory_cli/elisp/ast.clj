(ns spacetools.observatory-cli.elisp.ast
  "Emacs lisp AST."
  (:require [clojure.spec.alpha :as s]
            [medley.core :refer [deep-merge update-existing]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.parser :as parser]))


;;;; Values

(def prefix-nodes-tags
  "All tags of prefix nodes."
  #{:template :spread :quote :hole})

(def coll-nodes-tags
  "All tags of collections nodes."
  #{:list :vector})

(def terminal-nodes-tags
  "All tags of terminal nodes."
  #{:symbol :keyword :char :string :number})

(def key->wight
  "Map for sorting keys."
  {:tag 4 :prefix 3 :value 2 :children 1 :extra 0})


;;;; Specs

(s/def ::ast (s/map-of keyword? any?))


;;;; Predicates

(defn-spec terminal-node? boolean?
  "Returns true if X is a terminal node."
  [x any?]
  (some? (terminal-nodes-tags (first x))))


(defn-spec prefix-node? boolean?
  "Returns true if X is a prefix node."
  [x any?]
  (some? (prefix-nodes-tags (first x))))


(defn-spec coll-node? boolean?
  "Returns true if X is a collection node."
  [x any?]
  (some? (coll-nodes-tags (first x))))


;;;; Helpers

(def ast-node
  "Create ast node from KEY VAL pairs like in `hash-map`."
  (partial sorted-map-by (fn comp [a b] (apply > (map key->wight [a b])))))


(defn-spec inline-prefix ::ast
  "Add PREFIX to the inline prefixes of the NODE"
  [node ::ast prefix keyword?]
  (update node :prefix #(into [prefix] %)))


(defn-spec shrink-coll seqable?
  "Remove first and last element of COLL"
  [coll seqable?]
  (-> coll rest butlast))


(defn-spec ast-parent ::ast
  "Make parent AST onde with TAG and CHILDREN."
  [tag keyword? children seqable?]
  (ast-node :tag tag :children (vec children)))


(defmulti parse-tree->ast-visitor
  "Turns parse tree nodes into AST nodes."
  (fn [[tag]]
    {:pre  (keyword? tag)}
    (cond
      (get prefix-nodes-tags tag) :prefix-g
      (get terminal-nodes-tags tag) :terminal-g
      :else tag)))

(defmethod parse-tree->ast-visitor :prefix-g
  [[tag tok cont-or-tag & rst :as node]]
  (let [fn-q? (string? cont-or-tag)
        child (if fn-q? (first rst) cont-or-tag)]
    (inline-prefix child (if fn-q? :fn-quote tag))))

(defmethod parse-tree->ast-visitor :terminal-g
  [[tag value]]
  (ast-node :tag tag :value value))

(defmethod parse-tree->ast-visitor :root
  [[tag & rst]]
  (ast-parent tag rst))

(defmethod parse-tree->ast-visitor :vector
  [[tag & rst]]
  (ast-parent tag (shrink-coll rst)))

(defmethod parse-tree->ast-visitor :list
  [[tag & [_ fc sc :as rst]]]
  (if (and (= "quote" (:value fc))
           ((complement seq) (:prefix fc)))
    (inline-prefix sc :quote)
    (ast-parent tag (shrink-coll rst))))


(defn-spec walk-ast any?
  "Walk ast FORM applying PRE and POST to it: (POST (WALK (PRE NODE))).
NOTE: Return value of PRE should be mapable."
  [pre fn? post fn? form ::ast]
  (vary-meta
   (post (update-existing
          (pre form)
          :children #(mapv (partial walk-ast pre post) %)))
   deep-merge
   (meta form)))


(defn-spec parse-tree->ast ::ast
  "Convert parse tree into AST saving meta."
  [pt :spacetools.observatory-cli.elisp.parser/parse-tree]
  (parser/walk-parse-tree identity parse-tree->ast-visitor pt))

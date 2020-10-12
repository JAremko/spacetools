(ns spacetools.observatory-cli.elisp.ast
  "Emacs lisp AST."
  (:require [medley.core :refer [deep-merge update-existing]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.parser :as parser]
            [spacetools.observatory-cli.elisp.spec :as es]))

;; FIXME: Scrub comments and whitespaces by returning and removing {}.

;;;; Predicates

(defn-spec terminal-node? boolean?
  "Returns true if X is a terminal node."
  [x any?]
  (some? (es/terminal-node-tags (:tag x))))

(defn-spec prefix-node? boolean?
  "Returns true if X is a prefix node."
  [x any?]
  (some? (es/prefix-node-tags (:tag x))))

(defn-spec coll-node? boolean?
  "Returns true if X is a collection node."
  [x any?]
  (some? (es/coll-node-tags (:tag x))))

(defn-spec junk-node? boolean?
  "Returns true if X is a  junk node."
  [x any?]
  (some? (es/junk-node-tags (:tag x))))


;;;; Helpers

(defn-spec inline-prefix ::es/ast
  "Add PREFIX to the inline prefixes of the NODE"
  [node ::es/ast prefix keyword?]
  (update node :prefix #(into [prefix] %)))


(defn-spec shrink-coll seqable?
  "Remove first and last element of COLL"
  [coll seqable?]
  (-> coll rest butlast))


(defn-spec ast-parent ::es/ast
  "Make parent AST node with TAG and CHILDREN."
  [tag keyword? children seqable?]
  (->> children (remove nil?) vec (hash-map :tag tag :children)))


(defmulti parse-tree->ast-visitor
  "Turns parse tree nodes into AST nodes."
  (fn [[tag]]
    {:pre  (keyword? tag)}
    (cond
      (get es/prefix-node-tags tag) :prefix-g
      (get es/terminal-node-tags tag) :terminal-g
      (get es/junk-node-tags tag) :junk-g
      :else tag)))

(defmethod parse-tree->ast-visitor :junk-g [_] nil)

(defmethod parse-tree->ast-visitor :prefix-g
  [[tag tok cont-or-tag & rst :as node]]
  (let [fn-q? (string? cont-or-tag)
        child (if fn-q? (first rst) cont-or-tag)]
    (inline-prefix child (if fn-q? :fn-quote tag))))

(defmethod parse-tree->ast-visitor :terminal-g
  [[tag value]]
  {:tag tag :value value})

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
  [pre fn? post fn? form ::es/ast]
  (let [rv (post (update-existing
                  (pre form)
                  :children #(mapv (partial walk-ast pre post) %)))]
    (if (es/metable? rv)
      (vary-meta rv deep-merge (meta form))
      rv)))


(defn-spec parse-tree->ast ::es/ast
  "Convert parse tree into AST saving meta."
  [pt ::es/parse-tree]
  (parser/walk-parse-tree identity parse-tree->ast-visitor pt))

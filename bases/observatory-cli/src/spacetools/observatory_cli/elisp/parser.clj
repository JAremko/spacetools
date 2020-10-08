(ns spacetools.observatory-cli.elisp.parser
  "Emacs lisp parser."
  (:require [clj-antlr.core :as antlr]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [medley.core :refer [deep-merge]]
            [orchestra.core :refer [defn-spec]]))


(def grammar
  "Elisp grammar."
  (str/replace ;; FIXME: Replaces {{ANY}} with inline any element rule
   ;; to prevent wrapping. There should be a better way..
   "grammar EmacsLisp ;

   root: {{ANY}}* ;

   vector: '[' {{ANY}}* ']' ;

   list: '(' {{ANY}}* ')' ;

   quote: '#' ? '\\'' {{ANY}} ;

   template: '`' {{ANY}} ;

   spread: ',@' {{ANY}} ;

   hole: ',' {{ANY}} ;

   number: NUMB10 | NUMBX ;

   char: CHAR ;

   string: STRING ;

   keyword: KEYWORD ;

   symbol: IDENT ;

   whitespace: WS ;

   comment: COMLINE ;

   CHAR: '?' ( ( '\\\\' ( 'C' | 'M' ) '-' ) | '\\\\' )? . ;

   STRING: '\"' ( '\\\\\\\\' | '\\\\\"' | . )*? '\"' ;

   NUMB10: [+-] ? ( ( D* '.' D+ ) | ( D+ '.' D* ) | D+ ) ;

   NUMBX: '#' ( 'b' | 'o' | 'x' | ( D+ 'r' ) ) [-+]? ( A | D )+ ;

   fragment
   D: '0'..'9' ;

   fragment
   A: 'a'..'z' ;

   KEYWORD: ':' IDENT ;

   IDENT: ( ( '\\\\' [\\\\[\\]() \\n\\t\\r\"',`;] )+? |
            ( ~[[\\]() \\n\\t\\r\"',`;] )+? )+ ;

   COMLINE: ';' ~[\\n\\r]* /*{{JUNK}}*/;

   WS: [ \\n\\t\\r]+ /*{{JUNK}}*/;"

   "{{ANY}}"

   "( list | keyword | number | symbol | string | vector | char | whitespace |
      comment | quote | template | spread | hole )"))

(def grammar-prune
  "Version of EmacsLisp grammar that hides comments and white-spaces."
  (str/replace grammar "/*{{JUNK}}*/" " -> skip"))


;;;; Predicates

(defn-spec cons? boolean?
  "Returns true if X has type `clojure.lang.Cons`"
  [x any?]
  (= (type x) clojure.lang.Cons))


;;;; Specs

(s/def ::parse-tree (s/coll-of (s/or :terminal string? :child cons?)))


;;;; Helpers

(defn-spec walk-parse-tree any?
  "Walk parse tree FORM applying PRE and POST to it: (POST (WALK (PRE NODE))).
NOTE: Return value of PRE should be mapable."
  [pre fn? post fn? form ::parse-tree]
  (if ((some-fn string? keyword?) form)
    form
    (vary-meta (post (map (partial walk-parse-tree pre post) (pre form)))
               deep-merge
               (meta form))))


;;;; Parsers

(def elisp-str->pruned-parse-tree
  "Parses Emacs Lisp string into parse tree without white-spaces and comments."
  (antlr/parser grammar-prune))


(def elisp-str->full-parse-tree
  "Parses Emacs Lisp string into parse tree with comments and white-spaces."
  (antlr/parser grammar))


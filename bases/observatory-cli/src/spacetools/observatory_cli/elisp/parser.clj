(ns spacetools.observatory-cli.elisp.parser
  "Emacs lisp parser."
  (:require [clj-antlr.core :as antlr]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [medley.core :refer [deep-merge]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.observatory-cli.elisp.spec :as es]))


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


;;;; Helpers

(defn-spec walk-parse-tree any?
  "Walk parse tree FORM applying PRE and POST to it: (POST (WALK (PRE NODE))).
NOTE: Return value of PRE should be mapable."
  [pre fn? post fn? form ::es/parse-tree]
  (if ((some-fn string? keyword?) form)
    form
    (vary-meta (post (map (partial walk-parse-tree pre post) (pre form)))
               deep-merge
               (meta form))))


;;;; Parsers

(def parser (antlr/parser grammar))
(def parser-prune (antlr/parser grammar-prune))


(defn-spec elisp-str->pruned-parse-tree ::es/pruned-parse-tree
  "Parses Emacs Lisp string into parse tree without white-spaces and comments."
  [s string?]
  (parser-prune s))


(defn-spec elisp-str->full-parse-tree ::es/parse-tree
  "Parses Emacs Lisp string into parse tree with comments and white-spaces."
  [s string?]
  (parser s))

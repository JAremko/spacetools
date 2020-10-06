(ns spacetools.observatory-cli.parsel
  "Emacs lisp parser."
  (:require [clj-antlr.core :as antlr]
            [clojure.string :as str]))


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


(def elisp-str->pruned-parse-tree
  "Parses Emacs Lisp string into parse tree without white-spaces and comments."
  (antlr/parser grammar-prune))

(def elisp-str->full-parse-tree
  "Parses Emacs Lisp string into parse tree with comments and white-spaces."
  (antlr/parser grammar))


(def text
  "Test text"
  ";; (1 2 3) foo
`,@foo #'baz
,@()
(1.0)
(defvar configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout
  \"Timeout in seconds to reach a package archive page.\")
,bar
:zzz
\"fff\"
     ((file-exists-p layer-dir)
      (configuration-layer/message
       (concat \"Cannot create configuration layer \\\"\\\", \"
               \"this layer already exists.\") name))
1011;; baz
   ;; Note:
(1+
;; bar
)
(+1.0 +2 .0 0.0.0 #24r5 #b0.0 #b111 '() 2+2 2 '2 +1.2b [])
              (let ((a [1 2 3])) a)
?b ?  ?? ?\\C-m ?\\\\  ( ?\\\\ )
'[1 2 3]
 ((equal event 32) ?')   ; space 
or()
\"\\\\\\\\\" a
\"'\"
bar'(foo) (quote 10)
\\\\\\\\\\[foo
;")

;; (w/walk (map ) (elisp-str->pruned-parse-tree text))

;; (elisp-str->pruned-parse-tree text)

;; (defmulti sdn->org
;;   "Given node return its org-mode text representation."
;;   (fn [{tag :tag :as node}]
;;     {:pre  [((complement indirect-nodes) tag)
;;             (map? node)
;;             (keyword? tag)]}
;;     (cond
;;       ;; List node group.
;;       (#{:feature-list :plain-list} tag) :list

;;       ;; Emphasis containers.
;;       (#{:bold :italic :underline :strike-through} tag) :emphasis-container

;;       ;; Block-container node group.
;;       ((set (keys block-container-delims)) tag) :block-container

;;       ;; Everything else.
;;       :else tag)))


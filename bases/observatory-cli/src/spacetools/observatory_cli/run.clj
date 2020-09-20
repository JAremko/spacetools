(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [instaparse.core :as insta :refer [defparser]]
            [instaparse.combinators :as c]
            [clojure.string :as str]))


;;;; Rules:
(def root-s
  "Root rule."
  "<root> = ( comment | element | whitespace | Epsilon ) +")

(def comment-s
  "Comment line."
  "comment = comment-tok #';[^\\n]*|$'")

(def element-s
  "Any element rule."
  "<element> = ( sexp | vector | atom | expr ) +")

(def seqs-s
  "Sequences."
  "sexp   = sexp-l-tok ( coll-el ? | whitespace ? ) * sexp-r-tok
   vector = vec-l-tok (coll-el ? | whitespace ? ) * vec-r-tok")

(def coll-els-s
  "Sequence elements."
  "<coll-el> = sexp | vector | atom | expr | comment")

(def atoms-s
  "Terminal elements."
  "<atom>  = string | keyword | symbol | number

   symbol    = ! (kv-tok | l-tok | num-b-x-tok) ident
   keyword   = & kv-tok ident
   string    = <str-l-tok> #'(?:\\\\\"|[^\"])*' <str-r-tok>
   <num-b10> = #'[-+]?(?:(?:[\\d]*\\.[\\d]+)|(?:[\\d]+\\.[\\d]*)|(?:[\\d]+))' &
               end-tok
   <num-bx>  = #'(?i)#(?:b|o|x|(?:\\d+r))[-+]?[a-zA-Z0-9]+'
   number    = num-b10 | num-bx")

(def tokens-s
  "Markers of elements."
  "<sexp-l-tok>  = <'('>
   <sexp-r-tok>  = <')'>

   <vec-l-tok>   = <'['>
   <vec-r-tok>   = <']'>

   <str-l-tok>   = <'\"'>
   <str-r-tok>   = <str-l-tok>

   <comment-tok>   = <';'>

   <quote-tok>   = <'#'?> <\"'\">

   <tmpl-tok>    = <'`'>

   <num-b-x-tok> = '#'

   <hole-tok>    = <','> ! '@'

   <spread-tok>  = <',@'>

   <kv-tok>      = <':'>

   <whitespace> = <#'\\s+'>

   <new-line>    = <#'\n'>")

(def left-tokens-s
  "Tokens marking start of an element."
  "<l-tok> = sexp-l-tok | vec-l-tok | str-l-tok | quote-tok | tmpl-tok |
             hole-tok | spread-tok | kv-tok")

(def end-tokens-s
  "Tokens marking end of an element."
  "<end-tok> = sexp-r-tok | vec-r-tok | str-r-tok | quote-tok | tmpl-tok |
             hole-tok | spread-tok | whitespace | #'$' | comment")

(def expression-s
  "Expression rule."
  "<expr>   = quote | template | hole | spread

   quote    = quote-tok element
   template = tmpl-tok element
   hole     = hole-tok element
   spread   = spread-tok element")

(def ident
  "Ident rule."
  {:ident
   (let [esc-ch (str/join ["\\[" "\\]" "\\(" "\\)" "\"" "\\s" "'" "," "`"])
        tmpl "(?!;)(?:(?:\\\\[{{ec}}])|[^{{ec}}])+"]
    (->> esc-ch (str/replace tmpl "{{ec}}") c/regexp c/hide-tag))})

(defparser elisp-parser
  (->> [element-s seqs-s atoms-s tokens-s left-tokens-s expression-s
        coll-els-s root-s end-tokens-s comment-s]
       (mapv c/ebnf)
       (apply merge ident))
  :start :root
  :output-format :enlive)


;; (elisp-parser ";; (1 2 3) foo
;; \"fff\"
;;      ((file-exists-p layer-dir)
;;       (configuration-layer/message
;;        (concat \"Cannot create configuration layer \\\"\\\", \"
;;                \"this layer already exists.\") name))
;; 1011;; baz
;;    ;; Note:
;; (1+
;; ;; bar
;; )
;; (+1.0 +2 .0 0.0.0 #24r5 #b0.0 #b111 '() 2+2 2'2 +1.2b [])
;;               (let ((a [1 2 3])) a)
;; ;")

(defn -main [& args]
  (let [foo (slurp "/tmp/foo.el")]
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))
  (time (do (elisp-parser foo) "done"))))

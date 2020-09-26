(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [instaparse.core :as insta :refer [defparser]]
            [instaparse.combinators :as c]
            [clojure.string :as str]))


;;;; Rules:
(def root-s
  "Root."
  "<root> = any +")

(def any-s
  "Any element."
  "<any> = sexp | keyword | number | symbol | expr | string | vector |
           comment | whitespace | Epsilon")

(def comment-s
  "Comment line."
  "comment = #';(?:[^\\n]*|$)'")

(def string-s
  "String."
  "string = <str-l-tok> #'(?:\\\\\"|[^\"])*' <str-r-tok>")

(def whitespace-s
  "Whitespace."
  "<whitespace> = <#'\\s+'>")

(def seqs-s
  "Sequences."
  "sexp   = sexp-l-tok any + sexp-r-tok
   vector = vec-l-tok any + vec-r-tok")

(def expr-s
  "Expression (various quotes)."
  "<expr>   = quote | template | spread | hole

   <qtbl> = sexp | keyword | number | symbol | expr | vector

   quote    = quote-tok qtbl
   template = tmpl-tok qtbl
   hole     = hole-tok ! spread-tok qtbl
   spread   = hole-tok spread-tok qtbl")

(def tokens-s
  "Markers of elements."
  "<end-tok>     = sexp-r-tok | vec-r-tok | str-r-tok | quote-tok | tmpl-tok |
                   hole-tok | spread-tok | whitespace | comment | #'$'

   <sexp-l-tok>  = <'('>
   <sexp-r-tok>  = <')'>

   <vec-l-tok>   = <'['>
   <vec-r-tok>   = <']'>

   <str-l-tok>   = <'\"'>
   <str-r-tok>   = <'\"'>

   <quote-tok>   = '#' ? <\"'\">

   <tmpl-tok>    = <'`'>

   <num-b-x-tok> = '#'

   <hole-tok>    = <','>

   <spread-tok>  = <'@'>

   <kv-tok>      = <':'>")

(def atom-s
  "Terminal elements."
  "number    = num-b10 | num-bx
   <num-b10> = #'[-+]?(?:(?:[\\d]*\\.[\\d]+)|(?:[\\d]+\\.[\\d]*)|(?:[\\d]+))' &
               end-tok
   <num-bx>  = #'(?i)#(?:b|o|x|(?:\\d+r))[-+]?[a-z0-9]+'

   keyword   = kv-tok ident

   symbol    = ! number ident")

(def ident
  "Ident."
  {:ident
   (let [esc-ch (str/join ["\\[" "\\]" "\\(" "\\)" "\"" "\\s" "'" "," "`" ";"])
         tmpl "(?![;#:])(?:(?:\\\\[{{ec}}])|[^{{ec}}])+"]
     (c/hide-tag (c/regexp  (str/replace tmpl "{{ec}}" esc-ch))))})

(defparser elisp-parser
  (->> [seqs-s string-s tokens-s expr-s root-s comment-s whitespace-s any-s atom-s]
       (mapv c/ebnf)
       (apply merge ident))
  :start :root
  :output-format :enlive)

(defn -main [& args]
  (let [foo (slurp "/tmp/foo.el")]
    (prn (count (insta/parses elisp-parser foo)))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))
    (time (do (elisp-parser foo) "done"))))

;; (count (insta/parses elisp-parser (slurp "/tmp/foo.el")))

;; (insta/parses elisp-parser ";; (1 2 3) foo
;; `,@foo")

;; (insta/parses elisp-parser ";; (1 2 3) foo
;; `,@foo
;; (1.0)
;; (defvar configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout
;;   \"Timeout in seconds to reach a package archive page.\")
;; ,bar
;; :zzz
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

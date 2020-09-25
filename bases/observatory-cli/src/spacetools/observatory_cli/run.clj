(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [instaparse.core :as insta :refer [defparser]]
            [instaparse.combinators :as c]
            [clojure.string :as str]))


;;;; Rules:
(def root-s
  "Root rule."
  "<root> = any +")

(def any-s
  "Any element rule."
  "<any> = sexp | ident | expr | string | vector | comment | whitespace |
           Epsilon")

(def comment-s
  "Comment line."
  "comment = #';(?:[^\\n]*|$)'")

(def string-s
  "String rule."
  "string = <str-l-tok> #'(?:\\\\\"|[^\"])*' <str-r-tok>")

(def whitespace-s
  "Whitespace rule."
  "<whitespace> = <#'\\s+'>")

(def seqs-s
  "Sequences."
  "sexp   = sexp-l-tok any + sexp-r-tok
   vector = vec-l-tok any + vec-r-tok")

(def expr-s
  "Expression rule."
  "<expr>   = quote | template | ( spread / hole )

   <qtbl> = sexp | ident | expr

   quote    = quote-tok qtbl
   template = tmpl-tok qtbl
   hole     = hole-tok qtbl
   spread   = spread-tok qtbl")

(def tokens-s
  "Markers of elements."
  "<sexp-l-tok>  = <'('>
   <sexp-r-tok>  = <')'>

   <vec-l-tok>   = <'['>
   <vec-r-tok>   = <']'>

   <str-l-tok>   = <'\"'>
   <str-r-tok>   = <'\"'>

   <quote-tok>   = '#' ? <\"'\">

   <tmpl-tok>    = <'`'>

   <num-b-x-tok> = '#'

   <hole-tok>    = <','>

   <spread-tok>  = <',@'>

   <kv-tok>      = <':'>")

;; number    = num-b10 | num-bx
;; <num-b10> = #'[-+]?(?:(?:[\\d]*\\.[\\d]+)|(?:[\\d]+\\.[\\d]*)|(?:[\\d]+))'
;; <num-bx>  = #'(?i)#(?:b|o|x|(?:\\d+r))[-+]?[a-zA-Z0-9]+'

(def ident
  "Ident rule."
  {:ident
   (let [esc-ch (str/join ["\\[" "\\]" "\\(" "\\)" "\"" "\\s" "'" "," "`" ";"])
         tmpl "(?!;)(?:(?:\\\\[{{ec}}])|[^{{ec}}])+"]
     (c/regexp  (str/replace tmpl "{{ec}}" esc-ch)))})

(defparser first-stage-parser
  (->> [seqs-s string-s tokens-s expr-s root-s comment-s whitespace-s any-s]
       (mapv c/ebnf)
       (apply merge ident))
  :start :root
  :output-format :enlive)

(defn -main [& args]
  (let [foo (slurp "/tmp/foo.el")]
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))
  (time (do (first-stage-parser foo) "done"))))

(first-stage-parser ";; (1 2 3) foo
`,@foo
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
(+1.0 +2 .0 0.0.0 #24r5 #b0.0 #b111 '() 2+2 2'2 +1.2b [])
              (let ((a [1 2 3])) a)
;")

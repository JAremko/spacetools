(ns spacetools.observatory-cli.parsel
  "Elisp parser. Kinda really slow one. I made sure that there is no
  ambiguity and look back/ahead are at minimum but it still pretty slow.
  Howe slow? Well, it rougtly parses 1k of normal elisp lines per second.
  Yeah, it is pretty slow. Good thing that time seems to be linear.

  How can I make it go faster while still using instaparse:
  Basically deferring stuff - I can parse symbols keywords and numbers
  as the same \"thing\" - ident. It reduces the time by something like 30%.
  This is not that bad since I usually only need value of a ident, not type.
  Even better time saving (around 50%) can be achieved by avoiding
  expression parsing. It can be fine in most cases since quoted stuff is a
  value of something and usually I look for something specific and only
  then want to check its value. The only problem is that it will clutter
  the code and I don't think that making parser even 4 times faster
  radically changes anything.

  Maybe I should try replacing it with something similar like
  https://github.com/pest-parser/pest since it is \"native\" but the lib
  seems to struggle with recursive rules as well and I might have to
  implement line number metadata generator for it. Also I doubt it will
  be faster as a hole. I will have to output parsed .el as EDN string and
  re-read it into the rest of my stuff. it should add a lot of overhead.
  Overall I would call something like 10X time reduction a win but I doubt
  it can be achieved using this family of parsing strategy. Unless I did
  something extremely stupid.

  Does speed even matter? Not really :D in 80+% use cases I will parse
  a singe .el file that will be 100-300 lines long. But if the parser
  worked much much faster it might be viable option to parse all .el files
  in CI cases like detecting key binding collisions. Without the speed
  required I can do it only in branch updates since it would be too slow
  for PR CI. Workaround can be keeping cache of keybindings or parsed files.
  But passing something between CI jobs is a big pain in the rear end.
  Especially since I'll have to generate it on branch updates and access
  in PRs. Figuring out where to store it makes things even more difficult."
  (:require [clojure.string :as str]
            [instaparse.combinators :as c]
            [instaparse.core :as insta]
            [orchestra.core :refer [defn-spec]]))

;;; Rules:
(def root-s
  "Root."
  "<root> = any +")

(def any-s
  "Any element."
  "<any> = sexp | keyword | number | symbol | expr | string | vector |
           comment | whitespace | Epsilon")

(def comment-s
  "Comment line."
  "comment = comment-tok #'(?:[^\\n]*|$)'")

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

   <comment-tok> = <';'>

   <kv-tok>      = <':'>")

(def atom-s
  "Terminal elements."
  "number    = num-b10 | num-bx
   <num-b10> = #'[-+]?(?:(?:[\\d]*\\.[\\d]+)|(?:[\\d]+\\.[\\d]*)|(?:[\\d]+))' &
               end-tok
   <num-bx>  = #'(?i)#(?:b|o|x|(?:\\d+r))[-+]?[a-z0-9]+'

   keyword   = kv-tok ident

   symbol    = ! ( number | kv-tok | comment-tok | num-b-x-tok ) ident")

(def ident
  "Ident."
  {:ident
   (let [esc-ch (str/join ["\\[" "\\]" "\\(" "\\)" "\"" "\\s" "'" "," "`" ";"])
         tmpl "(?:(?:\\\\[{{ec}}])|[^{{ec}}])+"]
     (c/hide-tag (c/regexp  (str/replace tmpl "{{ec}}" esc-ch))))})

;;; Parser:
(insta/defparser ^{:doc "raw parser. Considered private."} elisp-parser
  (->> [seqs-s string-s tokens-s expr-s root-s comment-s whitespace-s any-s
        atom-s]
       (mapv c/ebnf)
       (apply merge ident))
  :start :root
  :output-format :enlive)


(defn-spec elisp-str->edn list?
  "Emacs Lisp parser."
  [s string?]
  (insta/transform
   {:comment (fn pad [text]
               (assoc {:tag :comment} :value (str ";" text)))
    :quote (fn add-fn-prop [f & [s]]
             (assoc {:tag :quote} :fn? (= f "#") :value (or s f)))}
   (insta/add-line-and-column-info-to-metadata s (elisp-parser s))))

#_ (do (def text
      "Test text"
      ";; (1 2 3) foo
`,@foo #'baz
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
(+1.0 +2 .0 0.0.0 #24r5 #b0.0 #b111 '() 2+2 2'2 +1.2b [])
              (let ((a [1 2 3])) a)
;")

    (elisp-str->edn text))
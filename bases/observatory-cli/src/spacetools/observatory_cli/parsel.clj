(ns spacetools.observatory-cli.parsel
  "Elisp parser. Kinda slow one. I ensured that there is no ambiguity,
  look back/ahead and recursive rules are at minimum but it still pretty slow.
  Howe slow? Well, it parses 1k of normal Elisp lines per ~700ms.
  Yeah, it is pretty slow. Good thing that time seems to be linear.

  Current trade-offs for better performance:
  I ended up deferring some work - symbols, keywords and numbers parsed
  as the same \"thing\" - ident-g (ident group). It reduces the time by ~30%.
  This is not that bad since I usually only need value of an ident, not type.

  Malformed code like \"2'2\" throws parser error instead of being
  treated as \"2 '2\"(Emacs does this) - It's ok because we want to detect
  such code. Emacs parses \"#b0.0\" as 2 numbers \"#b0\" and \".0\". The
  parser will interpreter this as a singe ident but will throw if you to
  parse it into a specific ident (symbol, number, keyword) so for the sake
  of code validation it makes sense to parse all idents.

  How can I make it go faster while still using instaparse:
  40%+ boost can be achieved by avoiding \"expression\" (quotes) parsing.
  It can be fine since quoted things usually are value and in the most cases
  I look for something specific and only then want to check its value.

  Maybe I should try replacing the whole thing with something similar, like
  https://github.com/pest-parser/pest since it is \"native\" but the lib
  seems to struggle with recursive rules as well. And I might have to
  implement line number metadata generator for it. Also I doubt it will
  be faster overall. I will have to output parsed .el as EDN string and
  re-read it into the rest of my stuff. it should add noticeable overhead.

  Does speed even matter? It does when I have to parse a lot of .el files
  in PR jobs. For example, to detect keybinding collisions.
  Workaround can be keeping cache of keybindings or parsed files.
  But passing something between CI jobs is a big pain in the rear end.
  Especially since I'll have to generate it on branch updates and access
  in PRs. Figuring out where to store it makes things even more difficult.

  Additionally compiling parser into a native-image makes it 3X times slower
  so I'll have to run it via JDK. Good thing TravisCI has runners with it
  pre-installed and I don't currently use this CI in PR jobs so I can run it
  concurrently with the rest of the CIs."
  (:require [clojure.string :as str]
            [instaparse.combinators :as c]
            [instaparse.core :as insta]
            [orchestra.core :refer [defn-spec]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: Rename expr rules into prefix and embed it into the child
;;        via optional prefix par. Basically, quotes will be a prop
;;        of lists and symbols. Don't forget to lift (quote ...)
;;        into a prop of its child. AND HANDLE THE CASE WHEN IT IS
;;        QUOTED. The lifting will have to be done at sexp node.
;;        and it also can be quoted... So I have to fold the quotes
;;        deep deep into the child.
;; NOTE:  VECTORS ARE ALSO QUOTABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Grammar:
(def roots-s
  "Root rules."
  "<root>         = any +
   <parsed-ident> = keyword | number | symbol")

(def any-s
  "Any element."
  "<any> = sexp | keyword | number | symbol | expr | string | vector |
           comment | whitespace | char | Epsilon")

(def comment-s
  "Comment line."
  "comment = comment-tok #'(?:[^\\n]*|$)'")

(def string-s
  "String."
  "string = <str-l-tok> #'(?:(?:\\\\\\\\)|(?:\\\\\")|[^\"])*' <str-r-tok>")

(def char-s
  "Char."
  "char = <char-tok> #'(?:(?:\\\\(?:C|M)-)|(?:\\\\))?(?:.|\\s)'")

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

   <qtbl> = sexp | symbol | keyword | number | expr | vector

   quote    = quote-tok qtbl
   template = tmpl-tok qtbl
   hole     = hole-tok ! spread-tok qtbl
   spread   = hole-tok spread-tok qtbl")

(def tokens-s
  "Markers of elements."
  "<end-tok>         = sexp-r-tok | sexp-l-tok | vec-r-tok | vec-l-tok |
                       whitespace | quote-tok | tmpl-tok | hole-tok | comment |
                       #'$'

   <sexp-l-tok>      = <'('>
   <sexp-r-tok>      = <')'>

   <vec-l-tok>       = <'['>
   <vec-r-tok>       = <']'>

   <str-l-tok>       = <'\"'>
   <str-r-tok>       = <'\"'>

   <quote-tok-not-f> = <\"'\">
   <quote-tok>       = '#' ? <quote-tok-not-f>

   <tmpl-tok>        = <'`'>

   <num-b-x-tok>     = '#'

   <hole-tok>        = <','>

   <spread-tok>      = <'@'>

   <comment-tok>     = <';'>

   <char-tok>        = '?'

   <kv-tok>          = <':'>")

(def idents-s
  "Ident group elements."
  "number    = num-b10 | num-bx
   <num-b10> = #'[-+]?(?:(?:[\\d]*\\.[\\d]+)|(?:[\\d]+\\.[\\d]*)|(?:[\\d]+))' &
               end-tok
   <num-bx>  = #'(?i)#(?:b|o|x|(?:\\d+r))[-+]?[a-z0-9]+'

   keyword   = kv-tok ident

   symbol    = ! ( number | kv-tok | comment-tok | num-b-x-tok | char-tok )
               ident")

(def ident
  "Ident."
  {:ident
   (let [esc-ch (str/join ["\\[" "\\]" "\\(" "\\)" "\"" "\\s" "'" "," "`" ";"])
         tmpl "(?:(?:\\\\[{{ec}}])|[^{{ec}}])+"]
     (->> esc-ch (str/replace tmpl "{{ec}}") c/regexp c/hide-tag))})

(def all-rules
  "All rules combined"
  (->> [seqs-s string-s tokens-s expr-s roots-s comment-s whitespace-s any-s
        idents-s char-s]
       (mapv c/ebnf)
       (apply merge ident)))

;;; Parsers:
(insta/defparser ^{:doc "raw elisp parser."} elisp-parser
  all-rules :start :root :output-format :enlive)

(insta/defparser ^{:doc "ident parser"} ident-parser
  all-rules :start :parsed-ident :output-format :enlive)

(defn-spec elisp-str->edn list?
  "Emacs Lisp parser."
  [s string?]
  (insta/transform
   {:comment (fn pad [text]
               (assoc {:tag :comment} :value (str ";" text)))
    :quote (fn add-fn-prop [f & [s]]
             (assoc {:tag :quote} :fn? (= f "#") :value (or s f)))}
   (insta/add-line-and-column-info-to-metadata s (elisp-parser s))
   ;; (elisp-parser s)
   ))

;; (ident-parser "#23rfffz")

;; (def text
;;   "Test text"
;;   ";; (1 2 3) foo
;; `,@foo #'baz
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
;; (+1.0 +2 .0 0.0.0 #24r5 #b0.0 #b111 '() 2+2 2 '2 +1.2b [])
;;               (let ((a [1 2 3])) a)
;; '[1 2 3]
;; or()
;; (?/ ?\\\\) ? ?
;; ??
;; ?\\C-s ?\\M-s ?\\M- fff
;; \"\\\\\"
;; \"'\"
;; bar'(foo)
;; ;")

;; (count (insta/parses elisp-parser text))

;; (insta/parse elisp-parser text)

;; (def path "/mnt/workspace/spacemacs-pr/layers/auto-layer.el")

;; (elisp-parser (slurp path))

;; ((shell-command-switches (cond
;;                            ((or(eq system-type 'darwin)
;;                                (eq system-type 'gnu/linux))
;;                             ;; execute env twice, once with a
;;                             ;; non-interactive login shell and
;;                             ;; once with an interactive shell
;;                             ;; in order to capture all the init
;;                             ;; files possible.
;;                             '("-lc" "-ic"))
;;                            ((eq system-type 'windows-nt) '("-c"))))
;;  (tmpfile (make-temp-file spacemacs-env-vars-file))
;;  (executable (cond ((or(eq system-type 'darwin)
;;                        (eq system-type 'gnu/linux)) "env")
;;                    ((eq system-type 'windows-nt) "set"))))

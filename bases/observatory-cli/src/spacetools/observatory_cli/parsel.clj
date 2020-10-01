(ns spacetools.observatory-cli.parsel
  "Elisp parser. Kinda slow one. I ensured that there is no ambiguity,
  look back/ahead and recursive rules are at minimum but it still pretty slow.
  It parses entire Spacemacs project (singe thread) in around one minute.

  Does speed even matter? It does when I have to parse a lot of .el files
  in PR jobs. For example, to detect keybinding collisions.
  Workaround can be keeping cache of keybindings or parsed files.
  But passing something between CI jobs is a big pain in the rear end.
  Especially since I'll have to generate it on branch updates and access
  in PRs. Figuring out where to store it makes things even more difficult.

  NOTE: compiling parser into a native-image makes it 3X times slower
  so I'll have to run it via JDK. Good thing TravisCI has runners with it
  pre-installed and I don't currently use this CI in PR jobs so I can run it
  concurrently with the rest of the CIs.

  NOTE: I tried parsing numbers keywords and symbols as idents - basically
  deffer the parse. It's ok sine I usually need ident value - not its type.
  Saved me ~30% parse time. I don't think this justifies additional complexity.

  NOTE: Also I tried to embed prefixes into prefixed elements but it didn't
  help since the grammar for them still has to be recursive."
  (:require [clojure.string :as str]
            [instaparse.combinators :as c]
            [instaparse.core :as insta]
            [spacetools.observatory-cli.parsel-util :refer [defrule]]
            [orchestra.core :refer [defn-spec]]))


;; TODO: PROMOTE (quote x)

;;; Grammar:
(defrule roots
  "Parser root."
  "<root> = any +")

(defrule element
  "Any element."
  "<any> = sexp | keyword | number | symbol | prefix | string | vector |
           comment | whitespace | char | Epsilon")

(defrule comment
  "Comment line."
  "comment = comment-tok #'(?:[^\\n]*|$)'")

(defrule string
  "String."
  "string = <str-l-tok> #'(?:(?:\\\\\\\\)|(?:\\\\\")|[^\"])*' <str-r-tok>")

(defrule char
  "Char."
  "char = <char-tok> #'(?:(?:\\\\(?:C|M)-)|(?:\\\\))?(?:.|\\s)'")

(defrule whitespace
  "Whitespace."
  "<whitespace> = <#'\\s+'>")

(defrule seqs
  "Sequences."
  "sexp   = sexp-l-tok any + sexp-r-tok
   vector = vec-l-tok any + vec-r-tok")

(defrule prefixes
  "Prefixes (various quotes)."
  "<prefix>   = quote | template | spread | hole

   <prfxbl> = sexp | symbol | keyword | number | prefix | vector

   quote    = quote-tok prfxbl
   template = tmpl-tok prfxbl
   hole     = hole-tok ! spread-tok prfxbl
   spread   = hole-tok spread-tok prfxbl")

(defrule tokens
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

(defrule symbol
  "Symbol."
  "symbol    = ! ( number | kv-tok | comment-tok | num-b-x-tok | char-tok )
               ident")

(defrule keyword
  "Keyword."
  "keyword = kv-tok ident")

(defrule number
  "Numbers."
  "number    = num-b10 | num-bx
   <num-b10> = #'[-+]?(?:(?:[\\d]*\\.[\\d]+)|(?:[\\d]+\\.[\\d]*)|(?:[\\d]+))' &
               end-tok
   <num-bx>  = #'(?i)#(?:b|o|x|(?:\\d+r))[-+]?[a-z0-9]+'")

(defrule ident
  "Ident part of symbols and keywords."
  {:ident
   (let [esc-ch (str/join ["\\[" "\\]" "\\(" "\\)" "\"" "\\s" "'" "," "`" ";"])
         tmpl "(?:(?:\\\\[{{ec}}])|[^{{ec}}])+"]
     (->> esc-ch (str/replace tmpl "{{ec}}") c/regexp c/hide-tag))})

(def all-rules
  "All rules combined"
  (merge ident seqs string tokens prefixes roots comment whitespace element
         char number keyword symbol))

;;; Parsers:
(insta/defparser ^{:doc "raw elisp parser."} elisp-parser
  all-rules :start :root :output-format :enlive)

(defn-spec elisp-str->edn list?
  "Emacs Lisp parser."
  [s string?]
  (insta/transform
   {:comment (fn pad [text]
               (assoc {:tag :comment} :value (str ";" text)))
    :quote (fn add-fn-prop [f & [s]]
             (assoc {:tag :quote} :fn? (= f "#") :value (or s f)))}
   (insta/add-line-and-column-info-to-metadata s (elisp-parser s))))

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

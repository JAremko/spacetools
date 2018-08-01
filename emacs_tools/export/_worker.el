;;; _worker.el ---  Spacemacs docs export worker -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; File structure was borrowed from ox-html.el by (Carsten Dominik
;; <carsten at orgmode dot org> and Jambunathan K
;; <kjambunathan at gmail dot com>).
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(when (and load-file-name
           noninteractive)
  (setq gc-cons-threshold 1000000000))

(eval-when-compile
  (require 'cl)
  (require 'url-util)
  (require 'subr-x))

(require 'ox)

(load-file
 (concat
  (file-name-directory
   (or load-file-name
       buffer-file-name))
  "../lib/toc-org.elc"))

(defvar sdnize-root-dir nil
  "Original root directory of the documentation.")

(declare-function toc-org-hrefify-gh "../lib/toc-org.el" (str &optional hash))

(defconst sdnize-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst sdnize-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")

(defconst sdnize-max-headline-level 5
  "Max level of headline nesting.")

(defconst sdnize-readme-template-url
  (concat "https://github.com/syl20bnr/spacemacs"
          "blob/develop/core/templates/README.org.template")
  "URL of README.org template")

(defconst sdnize-backend-funcs-alist
  '((bold . sdnize/bold)
    (center-block . sdnize/center-block)
    (clock . sdnize/clock)
    (code . sdnize/code)
    (drawer . sdnize/drawer)
    (dynamic-block . sdnize/dynamic-block)
    (entity . sdnize/entity)
    (example-block . sdnize/example-block)
    (export-block . sdnize/export-block)
    (export-snippet . sdnize/export-snippet)
    (fixed-width . sdnize/fixed-width)
    (footnote-definition . sdnize/footnote-definition)
    (footnote-reference . sdnize/footnote-reference)
    (headline . sdnize/headline)
    (horizontal-rule . sdnize/horizontal-rule)
    (inline-src-block . sdnize/inline-src-block)
    (inlinetask . sdnize/inlinetask)
    (inner-template . sdnize/inner-template)
    (italic . sdnize/italic)
    (item . sdnize/item)
    (keyword . sdnize/keyword)
    (latex-environment . sdnize/latex-environment)
    (latex-fragment . sdnize/latex-fragment)
    (line-break . sdnize/line-break)
    (link . sdnize/link)
    (node-property . sdnize/node-property)
    (paragraph . sdnize/paragraph)
    (plain-list . sdnize/plain-list)
    (plain-text . sdnize/plain-text)
    (planning . sdnize/planning)
    (property-drawer . sdnize/property-drawer)
    (quote-block . sdnize/quote-block)
    (radio-target . sdnize/radio-target)
    (section . sdnize/section)
    (special-block . sdnize/special-block)
    (src-block . sdnize/src-block)
    (statistics-cookie . sdnize/statistics-cookie)
    (strike-through . sdnize/strike-through)
    (subscript . sdnize/subscript)
    (superscript . sdnize/superscript)
    (table . sdnize/table)
    (table-cell . sdnize/table-cell)
    (table-row . sdnize/table-row)
    (target . sdnize/target)
    (template . sdnize/template)
    (timestamp . sdnize/timestamp)
    (underline . sdnize/underline)
    (verbatim . sdnize/verbatim)
    (verse-block . sdnize/verse-block))
  "plist of transcode functions names for `sdn' backend.")

(org-export-define-backend 'sdn
  sdnize-backend-funcs-alist
  :filters-alist
  '((:filter-final-output . sdnize/final-function)))


;;; Helper Functions

(defsubst sdnize/format-payload (format-string args)
  "Format payload for JSON."
  (replace-regexp-in-string
   "\n"
   "{{newline}}"
   (if args (apply 'format format-string args) format-string)))

(defsubst sdnize/export-file (src-file file-path)
  "Emit request for copying file at FILE-PATH. SRC-FILE will
be sent as the source of request (useful for debugging)"
  (message "{\"type\":\"export\",\"text\":%S,\"source\":%S}"
           (sdnize/format-payload
            file-path)
           (sdnize/format-payload
            src-file)))

(defsubst sdnize/message (format-string &rest args)
  "Emit specified message."
  (message "{\"type\":\"message\",\"text\":%S}"
           (sdnize/format-payload
            format-string
            args)))

(defsubst sdnize/warn (format-string &rest args)
  "Emit specified warning."
  (message "{\"type\":\"warning\",\"text\":%S}"
           (sdnize/format-payload
            format-string
            args)))

(defsubst sdnize/error (format-string &rest args)
  "Emit specified error and exit with code 1."
  (message "{\"type\":\"error\",\"text\":%S}"
           (sdnize/format-payload
            (concat (format "current-buffer: %s\n"
                            (buffer-name))
                    (format "current-file: %s\n"
                            (buffer-file-name))
                    "error: "
                    format-string)
            args))
  (kill-emacs 1))

(defconst sdnize/special-chars '(("\\" . "\\\\")
                                 ("\t" . "\\t")
                                 ("\r" . "\\r")
                                 ("\"" . "\\\"")
                                 ("\n" . "\\n")))

(defsubst sdnize/esc-str (str)
  "Escape special characters in STR."
  (if str
      (with-temp-buffer
        (insert str)
        (format-replace-strings sdnize/special-chars)
        (buffer-string))
    ""))

(defsubst sdnize/headline-make-path-id (headline)
  "Make id for org HEADLINE by chaining headlines from parent to
child headline.
NOTE: Each headline is converted with `toc-org-hrefify-gh' but
without unification and \"#\" prefix."
  (let* ((res nil)
         (cur-node headline)
         (parent-node (org-export-get-parent cur-node)))
    (cl-loop
     t
     (when (eq 'headline (car-safe cur-node))
       (push (string-remove-prefix
              "#"
              (toc-org-hrefify-gh
               (org-element-property
                :raw-value
                cur-node)))
             res))
     (if (not parent-node)
         (return res)
       (setq cur-node parent-node
             parent-node (org-export-get-parent cur-node))))
    (mapconcat 'identity res "/")))

(defun ask-user-about-lock (_ __)
  "Ignore locks on files"
  t)

(defun sdnize/noimpl (element)
  (sdnize/error "\"%s\" not implemented" element))


;;; Transcode Functions

;;;; Bold

(defun sdnize/bold (_bold contents _info)
  "Transcode BOLD From Org to Spacemacs SDN.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "{:tag :bold :children [%s]}" contents))

;;;; Center Block

(defun sdnize/center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element From Org to Spacemacs SDN.))
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "{:tag :center :children [%s]}" contents))

;;;; Clock

(defun sdnize/clock (_clock _contents _info)
  "Transcode a CLOCK element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (sdnize/noimpl "sdnize/clock"))

;;;; Code

(defun sdnize/code (code _contents _info)
  "Transcode CODE From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information.
NOTE: In Spacemacs ~code blocks~ are key sequences."
  (format "{:tag :kbd :value %s}"
          (format "%S"
                  (vconcat
                   (mapcar
                    'sdnize/esc-str
                    (split-string
                     (org-element-property :value code)
                     " "
                     "\\s*"))))))

;;;; Drawer

(defun sdnize/drawer (_drawer _contents _info)
  "Transcode a DRAWER element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/drawer"))

;;;; Dynamic Block

(defun sdnize/dynamic-block (_dynamic-block _contents _info)
  "Transcode a DYNAMIC-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (sdnize/noimpl "sdnize/dynamic-block"))

;;;; Entity

(defun sdnize/entity (_entity _contents _info)
  "Transcode an ENTITY object From Org to Spacemacs SDN.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (sdnize/noimpl "sdnize/entity"))

;;;; Example Block

(defun sdnize/example-block (example-block _contents _info)
  "Transcode a EXAMPLE-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{:tag :example :value \"%s\"}"
          (sdnize/esc-str (org-element-property :value example-block))))

;;;; Export Block

(defun sdnize/export-block (_export-block _contents _info)
  "Transcode a EXPORT-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/export-block"))

;;;; Export Snippet

(defun sdnize/export-snippet (_export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/export-snippet"))

;;;; Fixed Width

(defun sdnize/fixed-width (_fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/fixed-width"))

;;;; Footnote definition

(defun sdnize/footnote-definition
    (_footnote-definition _contents _info)
  "Transcode a FOOTNOTE-DEFINITION element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/footnote-definition"))

;;;; Footnote Reference

(defun sdnize/footnote-reference
    (_footnote-reference _contents _info)
  "Transcode a FOOTNOTE-REFERENCE element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/footnote-reference"))

;;;; Headline

(defun sdnize/headline (headline contents info)
  "Transcode a HEADLINE element From Org to Spacemacs SDN.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((raw-value (org-element-property :raw-value headline))
         (headline-ht (if (plist-member info :headline-hash)
                          (plist-get info :headline-hash)
                        (let ((hh (make-hash-table :test 'equal)))
                          (plist-put info :headline-hash hh)
                          hh)))
         (gh-id (toc-org-hrefify-gh raw-value headline-ht))
         (level (org-element-property :level headline))
         (path-ids (plist-get info :path-ids))
         (path-id
          (sdnize/headline-make-path-id headline))
         (file (plist-get info :input-file))
         (todo? (org-element-property :todo-keyword headline))
         (description? (and (= level 1)
                            (string= raw-value "Description"))))
    (unless (<= level sdnize-max-headline-level)
      (sdnize/error
       "File %S has headline %S with the nesting level %S - that's way too deep"
       file
       raw-value
       level))
    (unless (or todo? contents)
      (sdnize/error
       "File %S has headline %S without children or TODO marker"
       file
       raw-value))
    (when description?
      (if (plist-member info :file-has-description?)
          (sdnize/error
           (concat "File \"%s\" has multiply top level "
                   "\"Description\" headlines")
           file)
        (plist-put info :file-has-description? 'true)))
    (if (member path-id path-ids)
        (sdnize/error
         (concat "Multiply identical path IDs \"%s\" in %S file. "
                 "Usually it happens when headlines have child headlines "
                 "with similar names")
         path-id
         file)
      (plist-put info :path-ids (push path-id path-ids)))
    (puthash gh-id raw-value headline-ht)
    (format (concat "{:tag %s "
                    ":value \"%s\" "
                    ":level %s "
                    ":gh-id \"%s\" "
                    ":path-id \"%s\" "
                    ":children [%s]}")
            (cond (todo? :todo)
                  (description? :description)
                  (t (format "%s-level-%s" :headline level)))
            (sdnize/esc-str raw-value)
            level
            (sdnize/esc-str (string-remove-prefix "#" gh-id))
            (sdnize/esc-str path-id)
            contents)))

;;;; Horizontal Rule

(defun sdnize/horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object From Org to Spacemacs SDN.)))))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/horizontal-rule"))

;;;; Inline Src Block

(defun sdnize/inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (sdnize/noimpl "sdnize/inline-src-block"))

;;;; Inlinetask

(defun sdnize/inlinetask (_inlinetask _contents _info)
  "Transcode an INLINETASK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/inlinetask"))

;;;; Inner Template

(defun sdnize/inner-template (contents _info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "{:tag :body :children [%s]}" contents))

;;;; Italic

(defun sdnize/italic (_italic contents _info)
  "Transcode ITALIC From Org to Spacemacs SDN.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "{:tag :italic :children [%s]}" contents))

;;;; Item

(defun sdnize/item (item contents info)
  "Transcode an ITEM element From Org to Spacemacs SDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property :type (org-export-get-parent item)))
        (item-tag (org-element-property :tag item))
        (bullet (org-element-property :bullet item))
        (checkbox (org-element-property :checkbox item))
        (children (format "{:tag :item-children :children [%s]}" contents)))
    (unless (member type '(ordered unordered descriptive))
      (sdnize/error
       "File \"%s\" contains list item of type \"%s\" but it isn't implemented."
       (plist-get info :input-file)
       type))
    (format "{:tag :list-item :type :%s :bullet %s :checkbox %s :children [%s]}"
            type
            (format "\"%s\"" (sdnize/esc-str bullet))
            (when checkbox (format ":%s" (symbol-name checkbox)))
            (if item-tag
                (format "%s {:tag :item-tag :value %s}"
                        children
                        (if (char-or-string-p item-tag)
                            (format "\"%s\"" (sdnize/esc-str item-tag))
                          (org-export-data-with-backend item-tag 'sdn info)))
              children))))

;;;; Keyword

(defun sdnize/keyword (keyword _contents _info)
  "Transcode a KEYWORD element From Org to Spacemacs SDN.)))))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (val (org-element-property :value keyword)))
    (format "{:tag :key-word :key \"%s\" :value \"%s\"}"
            (sdnize/esc-str key)
            (sdnize/esc-str val))))

;;;; Latex Environment

(defun sdnize/latex-environment (_latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/latex-environment"))

;;;; Latex Fragment

(defun sdnize/latex-fragment (_latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/latex-fragment"))

;;;; Line Break

(defun sdnize/line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "{:tag :line-break}")

;;;; Link

(defconst sdnize/git-org-link-regexp
  (format (concat "\\/\\/github\\.com\\/%s\\/%s\\/blob"
                  "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")
          sdnize-repository-owner
          sdnize-repository))

(defconst sdnize/org-link-re ".+\\.org\\(\\(::\\|#\\| \\).*\\)?$")

(defsubst sdnize/fmt-link (path type raw-link desc)
  (format "{:tag :link :path \"%s\" :type :%s :raw-link \"%s\" :children [%s]}"
          (sdnize/esc-str path)
          (sdnize/esc-str type)
          (sdnize/esc-str raw-link)
          desc))

(defsubst sdnize/copy-if-asset (file raw-link path)
  ;; Errors:
  (cond
   ;; Missing target file.
   ((not (file-readable-p path))
    (sdnize/error
     "File %S has a link to file %S but it isn't readable."
     file
     (file-truename path)))
   ;; Target file is outside documentation root.
   ((not (string-prefix-p sdnize-root-dir
                          (file-truename path)))
    (sdnize/error
     (concat "File %S has a link to file %S "
             "but it's outside of the documentation root directory.")
     file
     (file-truename path))))
  ;; Copy assets.
  (unless (string-match-p sdnize/org-link-re raw-link)
    (sdnize/export-file file (file-truename path))))

(defun sdnize/link (link desc info)
  "Transcode a LINK object From Org to Spacemacs SDN.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (file (plist-get info :input-file))
         (raw-link (org-element-property :raw-link link))
         (file-path? (string= type "file"))
         (org-link-with-target? (and
                                 (string-match
                                  sdnize/org-link-re
                                  raw-link)
                                 (match-string 1 raw-link))))

    ;; Validation:
    (cond

     ;; Local .org file link with a target(anchor).
     ((and file-path? org-link-with-target?)
      (sdnize/error
       (concat "Link \"%s\" "
               "in \"%s\" "
               "should target the .org file at GitHub via web-link "
               "because it has target(anchors) and GitHub doesn't "
               "support them in ORG links :(\n"
               "(GitHub style anchors are supported)\n"
               "See footnote of %S for details.")
       raw-link
       file
       sdnize-readme-template-url))

     ;; Web link to org file inside Spacemacs GitHub repository.
     ((string-match sdnize/git-org-link-regexp raw-link)
      (let ((target-file (concat
                          sdnize-root-dir
                          (url-unhex-string (match-string 1 raw-link)))))
        (unless (file-readable-p target-file)
          (sdnize/error
           (concat
            "File %S has a GitHub link to a documentation file %S but "
            "it isn't readable locally.")
           file
           (file-truename target-file)))))

     ;; Catch the rest of known types.
     ((member type '("file" "http" "https" "custom-id" "ftp")))

     ;; Anything else is an error.
     (t (sdnize/error
         (concat
          "Link \"%s\" in file \"%s\" "
          "has type \"%s\" "
          "but the type isn't implemented in sdnize/link")
         raw-link
         file
         type)))

    (when file-path?
      (sdnize/copy-if-asset
       file
       raw-link
       (url-unhex-string path)))
    (sdnize/fmt-link path type raw-link desc)))

;;;; Node Property

(defun sdnize/node-property (_node-property _contents _info)
  "Transcode a NODE-PROPERTY element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/node-property"))

;;;; Paragraph

(defun sdnize/paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element From Org to Spacemacs SDN.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (format "{:tag :paragraph :children [%s]}" contents))

;;;; Plain List

(defsubst sdnize/plain-list-tag (plain-list file)
  "Return proper SDN tag for PLAIN-LIST or signal error."
  (let* ((type (org-element-property :type plain-list))
         (parent-type (symbol-name (car (org-export-get-parent plain-list))))
         (parent-hl (org-export-get-parent-headline plain-list))
         (parent-hl-parent-hl (org-export-get-parent-headline parent-hl))
         (perant-hl-val (org-element-property :raw-value parent-hl-parent-hl)))
    (unless (or (eq 'ordered type)
                (eq 'unordered type)
                (eq 'descriptive type))
      (sdnize/error (concat
                     "File \"%s\" contains plain list of type \"%s\" but "
                     "it isn't implemented.")
                    file
                    type))
    (if (and (not (string= parent-type "item"))
             (= (or (org-element-property :level parent-hl) -1) 2)
             (string= (org-element-property :raw-value parent-hl) "Features:")
             (= (or (org-element-property :level parent-hl-parent-hl) -1) 1)
             (string= perant-hl-val "Description"))
        :feature-list
      :plain-list)))

(defun sdnize/plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element From Org to Spacemacs SDN.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (file (plist-get info :input-file))
         (tag (sdnize/plain-list-tag plain-list file)))
    (when (eq tag :feature-list)
      (if (plist-member info :file-has-feature-list?)
          (sdnize/error
           (concat "File \"%s\" has multiply "
                   "\"Features:\" lists in the top "
                   "level \"Description\" headline")
           file)
        (plist-put info :file-has-feature-list? 'true)))
    (format "{:tag %s :type :%s :children [%s]}"
            tag
            type
            contents)))

;;;; Plain Text

(defun sdnize/plain-text (text _info)
  "Transcode a TEXT string From Org to Spacemacs SDN.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (format "{:tag :plain-text :value \"%s\"}"
          (sdnize/esc-str text)))

;;;; Planning

(defun sdnize/planning (_planning _contents _info)
  "Transcode a PLANNING element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (sdnize/noimpl "sdnize/planning"))

;;;; Property Drawer

(defun sdnize/property-drawer (_property-drawer _contents _info)
  "Transcode a PROPERTY-DRAWER element From Org to Spacemacs SDN.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/property-drawer"))

;;;; Quote Block

(defun sdnize/quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "{:tag :quote :children [%s]}" contents))

;;;; Radio Target

(defun sdnize/radio-target (_radio-target _text _info)
  "Transcode a RADIO-TARGET object From Org to Spacemacs SDN.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (sdnize/noimpl "sdnize/radio-target"))

;;;; Section

(defun sdnize/section (_section contents _info)
  "Transcode a SECTION element From Org to Spacemacs SDN.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (format "{:tag :section :children [%s]}" contents))

;;;; Special Block

(defun sdnize/special-block (_special-block _contents _info)
  "Transcode a SPECIAL-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/special-block"))

;;;; Src Block

(defun sdnize/src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (format "{:tag :src :language \"%s\" :value \"%s\"}"
          (sdnize/esc-str (org-element-property :language src-block))
          (sdnize/esc-str (org-element-property :value src-block))))

;;;; Statistics Cookie

(defun sdnize/statistics-cookie (_statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/statistics-cookie"))

;;;; Strike-Through

(defun sdnize/strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH From Org to Spacemacs SDN.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "{:tag :strike-through :children [%s]}" contents))

;;;; Subscript

(defun sdnize/subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object From Org to Spacemacs SDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "{:tag :subscript :children [%s]}" contents))

;;;; Superscript

(defun sdnize/superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object From Org to Spacemacs SDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "{:tag :superscript :children [%s]}" contents))

;;;; Table

(defun sdnize/table (table contents _info)
  "Transcode a TABLE element From Org to Spacemacs SDN.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property :type table)))
    (unless (eq type 'org)
      (sdnize/error
       "Table type \"%s\" isn't implemented in sdnize/table"
       type))
    (format "{:tag :table :type :%s :children [%s]}"
            (org-element-property :type table)
            contents)))

;;;; Table Cell

(defun sdnize/table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "{:tag :table-cell :children [%s]}" contents))

;;;; Table Row

(defun sdnize/table-row (table-row contents _info)
  "Transcode a TABLE-ROW element From Org to Spacemacs SDN.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (format "{:tag :table-row :type :%s :children [%s]}"
          (org-element-property :type table-row)
          contents))

;;;; Target

(defun sdnize/target (_target _contents _info)
  "Transcode a TARGET object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/target"))

;;;; Template

(defun sdnize/template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((file (file-truename (plist-get info :input-file))))
    (when (and (string-prefix-p (file-truename
                                 (concat sdnize-root-dir "layers/"))
                                file)
               (string-suffix-p "README.org" file t))
      (unless (plist-member info :file-has-description?)
        (sdnize/error
         "File \"%s\" doesn't have top level \"Description\" headline\n See %S"
         file
         sdnize-readme-template-url))
      (unless (plist-member info :file-has-feature-list?)
        (sdnize/error
         (concat "File \"%s\" "
                 "doesn't have \"Features:\"(With a colon) list in the "
                 "top level \"Description\" headline\n"
                 "See %S")
         file
         sdnize-readme-template-url))))
  (format "{:tag :root :headline-path-ids %s :children [%s]}"
          (map 'vector
               (lambda (s) (format "\"%s\"" (sdnize/esc-str s)))
               (plist-get info :path-ids))
          contents))

;;;; Timestamp

(defun sdnize/timestamp (_timestamp _contents _info)
  "Transcode a TIMESTAMP object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/timestamp"))

;;;; Underline

(defun sdnize/underline (_underline contents _info)
  "Transcode UNDERLINE From Org to Spacemacs SDN.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "{:tag :underline :children [%s]}" contents))

;;;; Verbatim

(defun sdnize/verbatim (verbatim _contents _info)
  "Transcode VERBATIM From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{:tag :verbatim :value \"%s\"}"
          (sdnize/esc-str (org-element-property :value verbatim))))

;;;; Verse Block

(defun sdnize/verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element From Org to Spacemacs SDN.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (format "{:tag :verse :children [%s]}" contents))


;;; Filter Functions

(defsubst sdnize/final-function-tidy (contents)
  "Filter to compact output by removing newline symbols.
FIXME: Figure out where they come from :"
  (replace-regexp-in-string "\n" "" contents))

(defsubst sdnize/final-function-fmt-vec-of-nil (str)
  "Replace [nil] with []."
  (replace-regexp-in-string "\\[nil\\]" "[]" str nil t))

(defsubst sdnize/final-function-lint (info)
  "Warn about potential errors."
  (let ((warnings (plist-get info :sdn-warnings)))
    (when (stringp warnings)
      (sdnize/warn
       "%s"
       (string-remove-suffix "\n" warnings)))))

(defun sdnize/final-function (contents _backend info)
  "Call final functions for `space-sdn' backend"
  (sdnize/final-function-lint info)
  (sdnize/final-function-fmt-vec-of-nil
   (sdnize/final-function-tidy contents)))


;;; End-user functions

(defun sdnize/to-sdn (root-dir exp-dir file-list)
  "Export org files in FILE-LIST into EXP-DIR.
SDNIZE-ROOT-DIR is original documentation root directory."
  (let* ((sdnize-root-dir (file-truename root-dir))
         (default-directory sdnize-root-dir))
    (dolist (in-file file-list)
      (let* ((out-file (concat exp-dir
                               (string-remove-suffix
                                ".org"
                                (string-remove-prefix
                                 sdnize-root-dir
                                 (file-truename in-file)))
                               ".sdn"))
             (out-dir (file-name-as-directory (file-name-directory out-file))))
        (unless (file-accessible-directory-p out-dir)
          (make-directory out-dir t))
        (sdnize/message "Exporting \"%s\" into \"%s\"" in-file out-file)
        (with-temp-buffer
          (find-file in-file)
          (org-export-to-file 'sdn out-file))
        (if (and (file-readable-p out-file)
                 (> (nth 7 (file-attributes out-file)) 0))
            (sdnize/message "Successfully exported \"%s\"" in-file)
          (sdnize/error "Done but \"%s\" doesn't exist or empty" out-file))))))

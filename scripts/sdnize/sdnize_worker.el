;;; sdnize_worker.el ---  Spacemacs SDN export worker -*- lexical-binding: t -*-
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

(defvar sdnize-root-dir nil
  "Original root directory of the documentation.")

(defconst sdnize-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst sdnize-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")

(defconst sdnize-max-headline-level 5
  "Max level of headline nesting.")

(defconst sdnize-readme-template-url
  (concat "https://github.com/syl20bnr/spacemacs"
          "/blob/develop/core/templates/README.org.template")
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


(defun sdnize/export-file (src-file file-path)
  "Emit request for copying file at FILE-PATH. SRC-FILE will
be sent as the source of request (useful for debugging)"
  (message "{\"type\":\"export\",\"text\":%S,\"source\":%S}"
           (sdnize/format-payload
            file-path)
           (sdnize/format-payload
            src-file)))

(defun sdnize/message (format-string &rest args)
  "Emit specified message."
  (message "{\"type\":\"message\",\"text\":%S}"
           (sdnize/format-payload
            format-string
            args)))

(defun sdnize/warn (format-string &rest args)
  "Emit specified warning."
  (message "{\"type\":\"warning\",\"text\":%S}"
           (sdnize/format-payload
            format-string
            args)))

(defun sdnize/error (format-string &rest args)
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
  "Get id for org HEADLINE by chaining headlines from parent to child headline."
  (let* ((res nil)
         (cur-node headline)
         (parent-node (org-export-get-parent cur-node)))
    (cl-loop
     t
     (when (eq 'headline (car-safe cur-node))
       (push (thread-last (org-element-property :raw-value cur-node)
               (downcase)
               (replace-regexp-in-string "[^[:alnum:]-]" " ")
               (replace-regexp-in-string "^\s+" "")
               (replace-regexp-in-string "\s+$" "")
               (replace-regexp-in-string "\s+" "_"))
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
CONTENTS is the text with bold markup. INFO is a plist holding
contextual information."
  (format "{:tag :bold :children [%s]}" contents))

;;;; Center Block

(defun sdnize/center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element From Org to Spacemacs SDN.))
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (format "{:tag :center :children [%s]}" contents))

;;;; Clock

(defun sdnize/clock (_clock _contents _info)
  "Transcode a CLOCK element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist used as a communication
channel."
  (sdnize/noimpl "sdnize/clock"))

;;;; Code

(defun sdnize/code (code _contents _info)
  "Transcode CODE From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual
information.
NOTE: In Spacemacs ~code blocks~ are key sequences."
  (format "{:tag :kbd :value %s}"
          (format "%s"
                  (vconcat
                   (mapcar
                    (lambda (el) (format "\"%s\"" (sdnize/esc-str el)))
                    (split-string
                     (org-element-property :value code)
                     " "
                     "\\s*"))))))

;;;; Drawer

(defun sdnize/drawer (_drawer _contents _info)
  "Transcode a DRAWER element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/drawer"))

;;;; Dynamic Block

(defun sdnize/dynamic-block (_dynamic-block _contents _info)
  "Transcode a DYNAMIC-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information.  See `org-export-data'."
  (sdnize/noimpl "sdnize/dynamic-block"))

;;;; Entity

(defun sdnize/entity (_entity _contents _info)
  "Transcode an ENTITY object From Org to Spacemacs SDN.
CONTENTS are the definition itself. INFO is a plist holding
contextual information."
  (sdnize/noimpl "sdnize/entity"))

;;;; Example Block

(defun sdnize/example-block (example-block _contents _info)
  "Transcode a EXAMPLE-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual
information."
  (format "{:tag :example :value \"%s\"}"
          (sdnize/esc-str (org-element-property :value example-block))))

;;;; Export Block

(defun sdnize/export-block (_export-block _contents _info)
  "Transcode a EXPORT-BLOCK element From Org to Spacemacs SDN.
  CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/export-block"))

;;;; Export Snippet

(defun sdnize/export-snippet (_export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object From Org to Spacemacs SDN.
  CONTENTS is nil. INFO is a plist holding contextual
  information."
  (sdnize/noimpl "sdnize/export-snippet"))

;;;; Fixed Width

(defun sdnize/fixed-width (_fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element From Org to Spacemacs SDN.
  CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/fixed-width"))

;;;; Footnote definition

(defun sdnize/footnote-definition
    (_footnote-definition _contents _info)
  "Transcode a FOOTNOTE-DEFINITION element From Org to Spacemacs SDN.
  CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/footnote-definition"))

;;;; Footnote Reference

(defun sdnize/footnote-reference
    (_footnote-reference _contents _info)
  "Transcode a FOOTNOTE-REFERENCE element From Org to Spacemacs SDN.
  CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/footnote-reference"))

;;;; Headline

(defun sdnize/headline (headline contents info)
  "Transcode a HEADLINE element From Org to Spacemacs SDN.
  CONTENTS holds the contents of the headline. INFO is a plist
  holding contextual information."
  (let* ((raw-val (org-element-property :raw-value headline))
         (level (org-element-property :level headline))
         (p-level-prop (thread-last headline
                         (org-export-get-parent)
                         (org-element-property :level)))
         (p-level (if (numberp p-level-prop) p-level-prop 0))
         (path-ids (plist-get info :path-ids))
         (path-id (sdnize/headline-make-path-id headline))
         (todo? (org-element-property :todo-keyword headline))
         (description? (and (= level 1) (string= raw-val "Description"))))

    ;; Validations:
    (cond
     ((not (= (- level p-level) 1))
      (sdnize/error "Headline %S has level %S but its parent level is %S"
                    raw-val
                    level
                    p-level))

     ((> level sdnize-max-headline-level)
      (sdnize/error "Headline %S has nesting level %S - max level is %S"
                    raw-val
                    level
                    sdnize-max-headline-level))

     ((not (or todo? contents))
      (sdnize/error "Empty headline %S without TODO marker" raw-val))

     ((and description? (plist-member info :file-has-description?))
      (sdnize/error "Multiply \"Description\" headlines"))

     ((member path-id path-ids)
      (sdnize/error (concat "Multiply identical path IDs \"%s\". "
                            "it can happen if two headlines have child "
                            "headlines with too similar names.")
                    path-id)))

    (plist-put info :path-ids (push path-id path-ids))
    (when description? (plist-put info :file-has-description? 'true))

    (format (concat "{:tag :headline "
                    ":todo? %s "
                    ":value \"%s\" "
                    ":path-id \"%s\" "
                    ":children [%s]}")
            (if todo? "true" "false")
            (sdnize/esc-str
             (if (plist-get info :with-smart-quotes)
                 (org-export-activate-smart-quotes raw-val :utf-8 info)
               raw-val))
            (sdnize/esc-str (sdnize/headline-make-path-id headline))
            contents)))

;;;; Horizontal Rule

(defun sdnize/horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object From Org to Spacemacs SDN.)))))
CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/horizontal-rule"))

;;;; Inline Src Block

(defun sdnize/inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (sdnize/noimpl "sdnize/inline-src-block"))

;;;; Inlinetask

(defun sdnize/inlinetask (_inlinetask _contents _info)
  "Transcode an INLINETASK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/inlinetask"))

;;;; Inner Template

(defun sdnize/inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (format "{:tag :root :source \"%s\" :spaceroot \"%s\" :children [%s]}"
          (sdnize/esc-str (file-truename (plist-get info :input-file)))
          (sdnize/esc-str (file-truename sdnize-root-dir))
          contents))

;;;; Italic

(defun sdnize/italic (_italic contents _info)
  "Transcode ITALIC From Org to Spacemacs SDN.
CONTENTS is the text with italic markup. INFO is a plist holding
contextual information."
  (format "{:tag :italic :children [%s]}" contents))

;;;; Item

(defun sdnize/item (item contents info)
  "Transcode an ITEM element From Org to Spacemacs SDN.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let ((type (org-element-property :type (org-export-get-parent item)))
        (item-tag (org-element-property :tag item))
        (bullet (org-element-property :bullet item))
        (checkbox (org-element-property :checkbox item))
        (children (format "{:tag :item-children :children [%s]}" contents)))
    (unless (member type '(ordered unordered descriptive))
      (sdnize/error "List item has type \"%s\" but it isn't implemented." type))
    (format "{:tag :list-item :type :%s :bullet %s :checkbox %s :children [%s]}"
            type
            (format "\"%s\"" (sdnize/esc-str bullet))
            (when checkbox (format ":%s" (symbol-name checkbox)))
            (if item-tag
                (format "%s {:tag :item-tag :children [%s]}"
                        children
                        (if (char-or-string-p item-tag)
                            (sdnize/plain-text item-tag info)
                          (org-export-data-with-backend item-tag 'sdn info)))
              children))))

;;;; Keyword

(defun sdnize/keyword (keyword _contents info)
  "Transcode a KEYWORD element From Org to Spacemacs SDN.)))))
CONTENTS is nil. INFO is a plist holding contextual information."
  (let* ((key (org-element-property :key keyword))
         (d-key (downcase key))
         (val (org-element-property :value keyword)))
    (when (string= "title" d-key)
      (if (plist-member info :doc-title)
          (sdnize/error "Multiply \"#+TITLE:\" keywords")
        (plist-put info :file-has-title? 'true)))
    (when (string= "tags" d-key)
      (if (plist-member info :doc-tags)
          (sdnize/error "Multiply \"#+TAGS:\" keywords")
        (plist-put info :file-has-tags? 'true)))
    (format "{:tag :key-word :key \"%s\" :value \"%s\"}"
            (sdnize/esc-str key)
            (sdnize/esc-str val))))

;;;; Latex Environment

(defun sdnize/latex-environment (_latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/latex-environment"))

;;;; Latex Fragment

(defun sdnize/latex-fragment (_latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/latex-fragment"))

;;;; Line Break

(defun sdnize/line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  "{:tag :line-break}")

;;;; Link

(defconst sdnize/git-org-link-regexp
  (format (concat "\\/\\/github\\.com\\/%s\\/%s\\/blob"
                  "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")
          sdnize-repository-owner
          sdnize-repository))

(defconst sdnize/org-link-re ".+\\.org\\(\\(::\\|#\\| \\).*\\)?$")

(defsubst sdnize/fmt-link (type path desc)
  (format "{:tag :link :type :%s :path \"%s\" :children [%s]}"
          (sdnize/esc-str type)
          (sdnize/esc-str (if (and (string= type "file")
                                   (not (string-prefix-p "file:" path t)))
                              (concat "file:" path)
                            path))
          desc))

(defsubst sdnize/copy-if-asset (file raw-link path)
  ;; Validate:
  (cond
   ;; Missing target file.
   ((not (file-readable-p path))
    (sdnize/error "Linked file %S not readable" (file-truename path)))
   ;; Target file is outside documentation root.
   ((not (string-prefix-p sdnize-root-dir (file-truename path)))
    (sdnize/error "link to file %S outside of root directory %S"
                  (file-truename path)
                  sdnize-root-dir)))
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
               "should target the .org file at GitHub via web-link "
               "because it has target(anchors) and GitHub doesn't "
               "support them in ORG links :(\n"
               "(GitHub style anchors are supported)\n"
               "See footnote of %S for details.")
       raw-link
       sdnize-readme-template-url))

     ;; Web link to org file inside Spacemacs GitHub repository.
     ((string-match sdnize/git-org-link-regexp raw-link)
      (let ((target-file (concat
                          sdnize-root-dir
                          (url-unhex-string (match-string 1 raw-link)))))
        (unless (file-readable-p target-file)
          (sdnize/error "link to remote Spacemacs file %S not readable locally"
                        (file-truename target-file)))))

     ;; Catch the rest of known types.
     ((member type '("file" "http" "https" "custom-id" "ftp")))

     ;; Anything else is an error.
     (t (sdnize/error "Link \"%s\" has unknown type \"%s\"" raw-link type)))

    (when file-path?
      (sdnize/copy-if-asset (plist-get info :input-file)
                            raw-link
                            (url-unhex-string path)))
    (sdnize/fmt-link type raw-link desc)))

;;;; Node Property


(defun sdnize/node-property (_node-property _contents _info)
  "Transcode a NODE-PROPERTY element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/node-property"))

;;;; Paragraph

(defun sdnize/paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element From Org to Spacemacs SDN.
CONTENTS is the contents of the paragraph, as a string. INFO is
the plist used as a communication channel."
  (format "{:tag :paragraph :children [%s]}" contents))

;;;; Plain List

(defsubst sdnize/plain-list-tag (plain-list)
  "Return proper SDN tag for PLAIN-LIST or signal error."
  (let* ((type (org-element-property :type plain-list))
         (parent-type (symbol-name (car (org-export-get-parent plain-list))))
         (parent-hl (org-export-get-parent-headline plain-list))
         (parent-hl-parent-hl (org-export-get-parent-headline parent-hl))
         (perant-hl-val (org-element-property :raw-value parent-hl-parent-hl)))
    (unless (or (eq 'ordered type)
                (eq 'unordered type)
                (eq 'descriptive type))
      (sdnize/error "Plain list type \"%s\" is unknown" type))
    (if (and (not (string= parent-type "item"))
             (= (or (org-element-property :level parent-hl) -1) 2)
             (string= (org-element-property :raw-value parent-hl) "Features:")
             (= (or (org-element-property :level parent-hl-parent-hl) -1) 1)
             (string= perant-hl-val "Description"))
        :feature-list
      :plain-list)))

(defun sdnize/plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element From Org to Spacemacs SDN.
CONTENTS is the contents of the list. INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (tag (sdnize/plain-list-tag plain-list)))
    (when (eq tag :feature-list)
      (if (plist-member info :file-has-feature-list?)
          (sdnize/error
           (concat "Multiply \"Features:\" lists in the top "
                   "level \"Description\" headline"))
        (plist-put info :file-has-feature-list? 'true)))
    (format "{:tag %s :type :%s :children [%s]}"
            tag
            type
            contents)))

;;;; Plain Text

(defun sdnize/plain-text (text info)
  "Transcode a TEXT string From Org to Spacemacs SDN.
TEXT is the string to transcode. INFO is a plist holding
contextual information."
  (if (not (string= "\n" text))
      (thread-last (if (plist-get info :with-smart-quotes)
                       (org-export-activate-smart-quotes text :utf-8 info)
                     text)
        (sdnize/esc-str)
        (format "{:tag :text :value \"%s\"}"))
    "{:tag :line-break}"))

;;;; Planning

(defun sdnize/planning (_planning _contents _info)
  "Transcode a PLANNING element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist used as a communication
channel."
  (sdnize/noimpl "sdnize/planning"))

;;;; Property Drawer

(defun sdnize/property-drawer (_property-drawer _contents _info)
  "Transcode a PROPERTY-DRAWER element From Org to Spacemacs SDN.
CONTENTS holds the contents of the drawer. INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/property-drawer"))

;;;; Quote Block

(defun sdnize/quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (format "{:tag :quoted :children [%s]}" contents))

;;;; Radio Target

(defun sdnize/radio-target (_radio-target _text _info)
  "Transcode a RADIO-TARGET object From Org to Spacemacs SDN.
TEXT is the text of the target. INFO is a plist holding
contextual information."
  (sdnize/noimpl "sdnize/radio-target"))

;;;; Section

(defun sdnize/section (_section contents _info)
  "Transcode a SECTION element From Org to Spacemacs SDN.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  (format "{:tag :section :children [%s]}" contents))

;;;; Special Block

(defun sdnize/special-block (_special-block _contents _info)
  "Transcode a SPECIAL-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (sdnize/noimpl "sdnize/special-block"))

;;;; Src Block

(defun sdnize/src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((lang (org-element-property :language src-block)))
    (unless lang
      (sdnize/error "Language not specified in \"#+BEGIN_SRC <language>\""))
    (format "{:tag :src :language \"%s\" :value \"%s\"}"
            (sdnize/esc-str lang)
            (sdnize/esc-str (org-element-property :value src-block)))))

;;;; Statistics Cookie

(defun sdnize/statistics-cookie (_statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (sdnize/noimpl "sdnize/statistics-cookie"))

;;;; Strike-Through

(defun sdnize/strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH From Org to Spacemacs SDN.
CONTENTS is the text with strike-through markup. INFO is a plist
holding contextual information."
  (format "{:tag :strike-through :children [%s]}" contents))

;;;; Subscript

(defun sdnize/subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object From Org to Spacemacs SDN.
CONTENTS is the contents of the object. INFO is a plist holding
contextual information."
  (format "{:tag :subscript :children [%s]}" contents))

;;;; Superscript

(defun sdnize/superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object From Org to Spacemacs SDN.
CONTENTS is the contents of the object. INFO is a plist holding
contextual information."
  (format "{:tag :superscript :children [%s]}" contents))

;;;; Table

(defun sdnize/table (table contents _info)
  "Transcode a TABLE element From Org to Spacemacs SDN.
CONTENTS is the contents of the table. INFO is a plist holding
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
CONTENTS is nil. INFO is a plist used as a communication
channel."
  (format "{:tag :table-cell :children [%s]}" contents))

;;;; Table Row

(defun sdnize/table-row (table-row contents _info)
  "Transcode a TABLE-ROW element From Org to Spacemacs SDN.
CONTENTS is the contents of the row. INFO is a plist used as a
communication channel."
  (format "{:tag :table-row :type :%s :children [%s]}"
          (org-element-property :type table-row)
          contents))

;;;; Target

(defun sdnize/target (_target _contents _info)
  "Transcode a TARGET object From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/target"))

;;;; Template

(defun sdnize/template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (let ((file (file-truename (plist-get info :input-file))))

    ;; Validations for layer README.org files:
    (when (and (string-prefix-p (file-truename
                                 (concat sdnize-root-dir "layers/"))
                                file)
               (string-suffix-p "README.org" file t))
      (unless (plist-member info :file-has-description?)
        (sdnize/error "Missing top level \"Description\" headline\n See %S"
                      sdnize-readme-template-url))
      (unless (plist-member info :file-has-feature-list?)
        (sdnize/error
         (concat "Missing \"Features:\"(With a colon) list in the "
                 "top level \"Description\" headline\n"
                 "See %S")
         sdnize-readme-template-url))))

  ;; General validations:
  (unless (plist-member info :file-has-title?)
    (sdnize/error "Missing \"#+TITLE:\" keyword. See %S"
                  sdnize-readme-template-url))
  (unless (plist-member info :file-has-tags?)
    (sdnize/warn "Missing \"#+TAGS:\" keyword. See %S"
                 sdnize-readme-template-url))

  ;; "Content is inner Template"
  contents)

;;;; Timestamp

(defun sdnize/timestamp (_timestamp _contents _info)
  "Transcode a TIMESTAMP object From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual
information."
  (sdnize/noimpl "sdnize/timestamp"))

;;;; Underline

(defun sdnize/underline (_underline contents _info)
  "Transcode UNDERLINE From Org to Spacemacs SDN.
CONTENTS is the text with underline markup. INFO is a plist
holding contextual information."
  (format "{:tag :underline :children [%s]}" contents))

;;;; Verbatim

(defun sdnize/verbatim (verbatim _contents _info)
  "Transcode VERBATIM From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual
information."
  (format "{:tag :verbatim :value \"%s\"}"
          (sdnize/esc-str (org-element-property :value verbatim))))

;;;; Verse Block

(defun sdnize/verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element From Org to Spacemacs SDN.
CONTENTS is verse block contents. INFO is a plist holding
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


;;; Documentation normalization

(defconst sdnize-prefmt-title-regexp "^#\\+TITLE:.*$")
(defconst sdnize-prefmt-begin-block-regexp "^#\\+BEGIN.*$")
(defconst sdnize-prefmt-end-block-regexp "^#\\+END.*$")
(defconst sdnize-prefmt-empty-line-regexp "^[ \t]*$")
(defconst sdnize-prefmt-tree-trunk-regexp "^[ 	]*|_")

(defsubst sdnize/nrmlz-rm-empty-lines-at-beg ()
  "Remove newlines at the beginning of the buffer."
  (goto-char (point-min))
  (while (looking-at-p sdnize-prefmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst sdnize/nrmlz-rm-empty-lines-at-end ()
  "Remove newlines at the ending of the buffer."
  (goto-char (point-max))
  (delete-blank-lines)
  (delete-blank-lines))

(defsubst sdnize/nrmlz-rm-trail-delim-in-hl ()
  "Remove trailing delimiters in headlines."
  (goto-char (point-min))
  (while (re-search-forward "^*+[[:space:]]+.*\\([;,\\.[:space:]]+\\)$" nil t)
    (replace-match "" nil nil nil 1)
    (forward-line -1)))

(defsubst sdnize/nrmlz-multy-nl-with-single ()
  "Replace multiply empty lines with a single empty line."
  (goto-char (point-min))
  (while (re-search-forward "\\(^[[:space:]]*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char)))

(defsubst sdnize/nrmlz-goto-next-table ()
  "Goto next org table.
Returns nil if no more tables left."
  (cl-loop
   ;; Skip current table.
   (goto-char (point-at-bol))
   (while (and (looking-at-p org-table-any-line-regexp)
               (not (= (point) (point-max))))
     (goto-char (point-at-bol))
     (forward-line))
   ;; Skip to the next table.
   (re-search-forward org-table-any-line-regexp nil t)
   (goto-char (point-at-bol))
   (unless (looking-at-p sdnize-prefmt-tree-trunk-regexp)
     (return)))
  (looking-at-p org-table-any-line-regexp))

(defsubst sdnize/nrmlz-remote-empty-lines-at-the-beginning ()
  "Remove empty lines at the begging of the buffer."
  (goto-char (point-min))
  (while (looking-at-p sdnize-prefmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst sdnize/nrmlz-insert-empty-line-after-title ()
  "Insert an empty line after title."
  (goto-char (point-min))
  (when (looking-at-p sdnize-prefmt-title-regexp)
    (forward-line 1)
    (unless (looking-at-p sdnize-prefmt-empty-line-regexp)
      (open-line 1))))

(defsubst sdnize/nrmlz-insert-empty-line-before-begin-block ()
  "Insert an empty line before begins of blocks."
  (goto-char (point-max))
  (while (re-search-backward sdnize-prefmt-begin-block-regexp nil t)
    (goto-char (point-at-bol))
    (forward-line -1)
    (unless (or (looking-at-p sdnize-prefmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (forward-line 1)
      (open-line 1))))

(defsubst sdnize/nrmlz-insert-empty-line-after-end-block ()
  "Insert an empty line after ends of blocks."
  (goto-char (point-min))
  (while (re-search-forward sdnize-prefmt-end-block-regexp nil t)
    (forward-line 1)
    (unless (looking-at-p sdnize-prefmt-empty-line-regexp)
      (open-line 1))))

(defsubst sdnize/nrmlz-insert-empty-line-at-the-end ()
  "Insert an empty line at the end of the buffer."
  (goto-char (point-max))
  (unless (looking-at-p sdnize-prefmt-empty-line-regexp)
    (open-line 1)))

(defsubst sdnize/nrmlz-insert-title ()
  "Insert #TITLE:{DIR_NAME} if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (looking-at-p sdnize-prefmt-title-regexp)
    (insert (format "#+TITLE:%s\n"
                    (file-name-base
                     (directory-file-name
                      (file-name-directory
                       (buffer-file-name))))))))

(defsubst sdnize/nrmlz-remove-empty-lines-after-headlines()
  "Remove empty liners after each headline."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (unless (= (forward-line) 0)
      (while (looking-at-p sdnize-prefmt-empty-line-regexp)
        (delete-blank-lines)))))

(defsubst sdnize/nrmlz-insert-empty-line-before-tables ()
  "Insert an empty line before each org table."
  (goto-char (point-min))
  (while (sdnize/nrmlz-goto-next-table)
    (forward-line -1)
    (unless (looking-at-p sdnize-prefmt-empty-line-regexp)
      (end-of-line)
      (open-line 1))
    (forward-line 1)))

(defsubst sdnize/nrmlz-insert-empty-line-after-sections ()
  "Insert an empty line after each section."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (forward-line -1)
    (unless (or (looking-at-p sdnize-prefmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (end-of-line)
      (open-line 1))
    (forward-line 2)))

(defsubst sdnize/nrmlz-insert-empty-line-after-tables ()
  "Insert an empty line after each table."
  (goto-char (point-min))
  (while (sdnize/nrmlz-goto-next-table)
    ;; Skip current table.
    (while (looking-at-p org-table-any-line-regexp)
      (forward-line))
    (unless (looking-at-p sdnize-prefmt-empty-line-regexp)
      (goto-char (point-at-bol))
      (open-line 1)
      (forward-line))))

(defsubst sdnize/nrmlz-align-tables ()
  "Align all tables"
  (goto-char (point-min))
  (while (sdnize/nrmlz-goto-next-table)
    (ignore-errors
      (org-table-align))))

(defun sdnize/nrmlz-apply-all ()
  "Format current `org-mode' buffer."
  (let ((old-buff-str (buffer-string))
        (new-buff-str ""))
    (cl-loop
     (sdnize/nrmlz-rm-empty-lines-at-beg)
     (sdnize/nrmlz-rm-empty-lines-at-end)
     (sdnize/nrmlz-rm-trail-delim-in-hl)
     (sdnize/nrmlz-multy-nl-with-single)
     (sdnize/nrmlz-remote-empty-lines-at-the-beginning)
     (sdnize/nrmlz-insert-title)
     (sdnize/nrmlz-remove-empty-lines-after-headlines)
     (sdnize/nrmlz-insert-empty-line-before-tables)
     (sdnize/nrmlz-insert-empty-line-after-title)
     (sdnize/nrmlz-insert-empty-line-after-tables)
     (sdnize/nrmlz-insert-empty-line-after-sections)
     (sdnize/nrmlz-insert-empty-line-before-begin-block)
     (sdnize/nrmlz-insert-empty-line-after-end-block)
     (sdnize/nrmlz-insert-empty-line-at-the-end)
     (sdnize/nrmlz-align-tables)
     (setq new-buff-str (buffer-string))
     (if (string= old-buff-str new-buff-str)
         (return)
       (setq old-buff-str new-buff-str)))))


;;; End-user functions

(defun sdnize/to-sdn (root-dir exp-dir file-list)
  "Export org files in FILE-LIST into EXP-DIR.
ROOT-DIR is original documentation root directory."
  (cl-letf* ((sdnize-root-dir (file-truename root-dir))
             (org-src-preserve-indentation t)
             (org-export-with-smart-quotes t)
             (org-export-with-sub-superscripts nil)
             (create-lockfiles nil)
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
        (if (not (string-match-p
              "[^[:space:]]"
              (with-temp-buffer
                (insert-file-contents in-file)
                (buffer-string))))
            (sdnize/error "Input \"%s\" is empty" in-file)
          (unless (file-accessible-directory-p out-dir)
            (make-directory out-dir t))
          (sdnize/message "Exporting \"%s\" into \"%s\"" in-file out-file)
          (with-temp-buffer
            (find-file in-file)
            (sdnize/nrmlz-apply-all)
            (org-export-to-file 'sdn out-file))
          (if (and (file-readable-p out-file)
                   (> (nth 7 (file-attributes out-file)) 0))
              (sdnize/message "Successfully exported \"%s\"" in-file)
            (sdnize/error "Done but \"%s\" doesn't exist or empty"
                          out-file)))))))

(provide 'sdnize_worker)

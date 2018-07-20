;;; _worker.el ---  Spacemacs docs export worker -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
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

(defvar spacemacs--root-dir nil
  "Original root directory of the documentation.")

(declare-function toc-org-hrefify-gh "../lib/toc-org.el" (str &optional hash))

(defconst spacemacs-repository "spacemacs"
  "Name of the Spacemacs remote repository.")
(defconst spacemacs-repository-owner "syl20bnr"
  "Name of the Spacemacs remote repository owner.")

(defconst spacemacs-max-headline-level 5
  "Max level of headline nesting.")

(defconst spacemacs-readme-template-url
  (concat "https://github.com/syl20bnr/spacemacs/"
          "blob/develop/core/templates/README.org.template")
  "URL of README.org template")

(defvar spacemacs--org-export-define-backend-funcs-alist
  '((bold . spacemacs//org-sdn-bold)
    (center-block . spacemacs//org-sdn-center-block)
    (clock . spacemacs//org-sdn-clock)
    (code . spacemacs//org-sdn-code)
    (drawer . spacemacs//org-sdn-drawer)
    (dynamic-block . spacemacs//org-sdn-dynamic-block)
    (entity . spacemacs//org-sdn-entity)
    (example-block . spacemacs//org-sdn-example-block)
    (export-block . spacemacs//org-sdn-export-block)
    (export-snippet . spacemacs//org-sdn-export-snippet)
    (fixed-width . spacemacs//org-sdn-fixed-width)
    (footnote-definition . spacemacs//org-sdn-footnote-definition)
    (footnote-reference . spacemacs//org-sdn-footnote-reference)
    (headline . spacemacs//org-sdn-headline)
    (horizontal-rule . spacemacs//org-sdn-horizontal-rule)
    (inline-src-block . spacemacs//org-sdn-inline-src-block)
    (inlinetask . spacemacs//org-sdn-inlinetask)
    (inner-template . spacemacs//org-sdn-inner-template)
    (italic . spacemacs//org-sdn-italic)
    (item . spacemacs//org-sdn-item)
    (keyword . spacemacs//org-sdn-keyword)
    (latex-environment . spacemacs//org-sdn-latex-environment)
    (latex-fragment . spacemacs//org-sdn-latex-fragment)
    (line-break . spacemacs//org-sdn-line-break)
    (link . spacemacs//org-sdn-link)
    (node-property . spacemacs//org-sdn-node-property)
    (paragraph . spacemacs//org-sdn-paragraph)
    (plain-list . spacemacs//org-sdn-plain-list)
    (plain-text . spacemacs//org-sdn-plain-text)
    (planning . spacemacs//org-sdn-planning)
    (property-drawer . spacemacs//org-sdn-property-drawer)
    (quote-block . spacemacs//org-sdn-quote-block)
    (radio-target . spacemacs//org-sdn-radio-target)
    (section . spacemacs//org-sdn-section)
    (special-block . spacemacs//org-sdn-special-block)
    (src-block . spacemacs//org-sdn-src-block)
    (statistics-cookie . spacemacs//org-sdn-statistics-cookie)
    (strike-through . spacemacs//org-sdn-strike-through)
    (subscript . spacemacs//org-sdn-subscript)
    (superscript . spacemacs//org-sdn-superscript)
    (table . spacemacs//org-sdn-table)
    (table-cell . spacemacs//org-sdn-table-cell)
    (table-row . spacemacs//org-sdn-table-row)
    (target . spacemacs//org-sdn-target)
    (template . spacemacs//org-sdn-template)
    (timestamp . spacemacs//org-sdn-timestamp)
    (underline . spacemacs//org-sdn-underline)
    (verbatim . spacemacs//org-sdn-verbatim)
    (verse-block . spacemacs//org-sdn-verse-block))
  "plist of transcode functions names for `spacemacs-sdn' backend.")

(org-export-define-backend 'spacemacs-sdn
  spacemacs--org-export-define-backend-funcs-alist
  :filters-alist
  '((:filter-final-output . spacemacs//org-sdn-final-function)))


;;; Helper Functions

(defsubst spacemacs//org-sdn-format-payload (format-string args)
  "Format payload for JSON."
  (replace-regexp-in-string
   "\n"
   "\r"
   (if args (apply 'format format-string args) format-string)))

(defsubst spacemacs/org-sdn-export-file (src-file file-path)
  "Emit request for copying file at FILE-PATH. SRC-FILE will
be sent as the source of request (useful for debugging)"
  (message "{\"type\":\"export\",\"text\":%S,\"source\":%S}"
           (spacemacs//org-sdn-format-payload
            file-path)
           (spacemacs//org-sdn-format-payload
            src-file)))

(defsubst spacemacs/org-sdn-message (format-string &rest args)
  "Emit specified message."
  (message "{\"type\":\"message\",\"text\":%S}"
           (spacemacs//org-sdn-format-payload
            format-string
            args)))

(defsubst spacemacs/org-sdn-warn (format-string &rest args)
  "Emit specified warning."
  (message "{\"type\":\"warning\",\"text\":%S}"
           (spacemacs//org-sdn-format-payload
            format-string
            args)))

(defsubst spacemacs/org-sdn-error (format-string &rest args)
  "Emit specified error and exit with code 1."
  (message "{\"type\":\"error\",\"text\":%S}"
           (spacemacs//org-sdn-format-payload
            (concat (format "current-buffer: %s\n"
                            (buffer-name))
                    (format "current-file: %s\n"
                            (buffer-file-name))
                    "error: "
                    format-string)
            args))
  (kill-emacs 1))

(defconst spacemacs-org-sdn-special-chars '(("\\" . "\\\\")
                                            ("\t" . "\\t")
                                            ("\r" . "\\r")
                                            ("\"" . "\\\"")
                                            ("\n" . "\\n")))
(defsubst spacemacs/org-sdn-escape-string (str)
  "Escape special characters in STR."
  (if str
      (with-temp-buffer
        (insert str)
        (format-replace-strings spacemacs-org-sdn-special-chars)
        (buffer-string))
    ""))

(defsubst spacemacs/org-sdn-headline-make-path-id (headline)
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


;;; Transcode Functions

;;;; Bold

(defun spacemacs//org-sdn-bold (_bold contents _info)
  "Transcode BOLD From Org to Spacemacs SDN.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "{:tag :bold :children [%s]}" contents))

;;;; Center Block

(defun spacemacs//org-sdn-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element From Org to Spacemacs SDN.))
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "{:tag :center :children [%s]}" contents))

;;;; Clock

(defun spacemacs//org-sdn-clock (_clock _contents _info)
  "Transcode a CLOCK element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-clock")
  "")

;;;; Code

(defun spacemacs//org-sdn-code (code _contents _info)
  "Transcode CODE From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information.
NOTE: In Spacemacs ~code blocks~ are key sequences."
  (format "{:tag :kbd :value %s}"
          (format "%S"
                  (vconcat
                   (mapcar
                    'spacemacs/org-sdn-escape-string
                    (split-string
                     (org-element-property :value code)
                     " "
                     "\\s*"))))))

;;;; Drawer

(defun spacemacs//org-sdn-drawer (_drawer _contents _info)
  "Transcode a DRAWER element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-drawer")
  "")

;;;; Dynamic Block

(defun spacemacs//org-sdn-dynamic-block (_dynamic-block _contents _info)
  "Transcode a DYNAMIC-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-dynamic-block")
  "")

;;;; Entity

(defun spacemacs//org-sdn-entity (_entity _contents _info)
  "Transcode an ENTITY object From Org to Spacemacs SDN.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-entity")
  "")

;;;; Example Block

(defun spacemacs//org-sdn-example-block (example-block _contents _info)
  "Transcode a EXAMPLE-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{:tag :example :value \"%s\"}"
          (spacemacs/org-sdn-escape-string
           (org-element-property :value example-block))))

;;;; Export Block

(defun spacemacs//org-sdn-export-block (_export-block _contents _info)
  "Transcode a EXPORT-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-export-block")
  "")

;;;; Export Snippet

(defun spacemacs//org-sdn-export-snippet (_export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-export-snippet")
  "")

;;;; Fixed Width

(defun spacemacs//org-sdn-fixed-width (_fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-fixed-width")
  "")

;;;; Footnote Reference

(defun spacemacs//org-sdn-footnote-reference
    (_footnote-reference _contents _info)
  "Transcode a FOOTNOTE-REFERENCE element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-footnote-reference")
  "")

;;;; Headline

(defun spacemacs//org-sdn-headline (headline contents info)
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
          (spacemacs/org-sdn-headline-make-path-id headline))
         (file (plist-get info :input-file))
         (todo? (org-element-property :todo-keyword headline))
         (description? (and (= level 1)
                            (string= raw-value "Description"))))
    (unless (<= level spacemacs-max-headline-level)
      (spacemacs/org-sdn-error
       "File %S has headline %S with the nesting level %S - that's way too deep"
       file
       raw-value
       level))
    (unless (or todo? contents)
      (spacemacs/org-sdn-error
       "File %S has headline %S without children or TODO marker"
       file
       raw-value))
    (when description?
      (if (plist-member info :file-has-description?)
          (spacemacs/org-sdn-error
           (concat "File \"%s\" has multiply top level "
                   "\"Description\" headlines")
           file)
        (plist-put info :file-has-description? 'true)))
    (if (member path-id path-ids)
        (spacemacs/org-sdn-error
         (concat "Multiply identical path IDs \"%s\" in %S file. "
                 "Usually it happens when headlines have child headlines "
                 "with similar names")
         path-id
         file)
      (plist-put info :path-ids (push path-id path-ids)))
    (puthash gh-id raw-value headline-ht)
    (format
     (concat "{:tag %s "
             ":value \"%s\" "
             ":level %s "
             ":gh-id \"%s\" "
             ":path-id \"%s\" "
             ":children [%s]}")
     (cond
      (todo? :todo)
      (description? :description)
      (t (format "%s-level-%s" :headline level)))
     (spacemacs/org-sdn-escape-string raw-value)
     level
     (spacemacs/org-sdn-escape-string (string-remove-prefix "#" gh-id))
     (spacemacs/org-sdn-escape-string path-id)
     contents)))

;;;; Horizontal Rule

(defun spacemacs//org-sdn-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object From Org to Spacemacs SDN.)))))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-horizontal-rule")
  "")

;;;; Inline Src Block

(defun spacemacs//org-sdn-inline-src-block (_inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-inline-src-block")
  "")

;;;; Inlinetask

(defun spacemacs//org-sdn-inlinetask (_inlinetask _contents _info)
  "Transcode an INLINETASK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-inlinetask")
  "")

;;;; Inner Template

(defun spacemacs//org-sdn-inner-template (contents _info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "{:tag :body :children [%s]}" contents))

;;;; Italic

(defun spacemacs//org-sdn-italic (_italic contents _info)
  "Transcode ITALIC From Org to Spacemacs SDN.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "{:tag :italic :children [%s]}" contents))

;;;; Item

(defun spacemacs//org-sdn-item (item contents info)
  "Transcode an ITEM element From Org to Spacemacs SDN.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property
               :type
               (org-export-get-parent item)))
        (item-tag (org-element-property :tag item))
        (checkbox (org-element-property :checkbox item))
        (children (format "{:tag :item-children :children [%s]}" contents)))
    (unless (or (eq 'ordered type)
                (eq 'unordered type)
                (eq 'descriptive type))
      (spacemacs/org-sdn-error
       (concat "File \"%s\" contains plain list item of type \"%s\" but "
               "it isn't implemented in spacemacs//org-sdn-item")
       (plist-get info :input-file)
       type))
    (format (concat "{:tag :list-item "
                    ":type :%s "
                    ":bullet %s "
                    ":checkbox %s "
                    ":children [%s]}")
            type
            (format
             "\"%s\""
             (spacemacs/org-sdn-escape-string
              (org-element-property :bullet item)))
            (when checkbox (format ":%s" (symbol-name checkbox)))
            (if item-tag
                (format
                 "%s {:tag :item-tag :value %s}"
                 children
                 (if (char-or-string-p item-tag)
                     (format "\"%s\""
                             (spacemacs/org-sdn-escape-string item-tag))
                   (org-export-data-with-backend item-tag
                                                 'spacemacs-sdn
                                                 info)))
              children))))

;;;; Keyword

(defun spacemacs//org-sdn-keyword (keyword _contents _info)
  "Transcode a KEYWORD element From Org to Spacemacs SDN.)))))
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "{:tag :key-word :key \"%s\" :value \"%s\"}"
          (spacemacs/org-sdn-escape-string
           (org-element-property :key keyword))
          (spacemacs/org-sdn-escape-string
           (org-element-property :value keyword))))

;;;; Latex Environment

(defun spacemacs//org-sdn-latex-environment (_latex-environment _contents _info)
  "Transcode a LATEX-ENVIRONMENT element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-latex-environment")
  "")

;;;; Latex Fragment

(defun spacemacs//org-sdn-latex-fragment (_latex-fragment _contents _info)
  "Transcode a LATEX-FRAGMENT object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-latex-fragment")
  "")

;;;; Line Break

(defun spacemacs//org-sdn-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "{:tag :line-break}")

;;;; Link

(defconst spacemacs--org-sdn-spacemacs-git-org-link-regexp
  (format (concat "\\/\\/github\\.com\\/%s\\/%s\\/blob"
                  "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")
          spacemacs-repository-owner
          spacemacs-repository))


(defconst spacemacs--org-sdn-org-link-re ".+\\.org\\(\\(::\\|#\\| \\).*\\)?$")


(defsubst spacemacs//org-snd-fmt-output (path type raw-link desc)
  (format "{:tag :link :path \"%s\" :type :%s :raw-link \"%s\" :children [%s]}"
          (spacemacs/org-sdn-escape-string path)
          (spacemacs/org-sdn-escape-string type)
          (spacemacs/org-sdn-escape-string raw-link)
          desc))


(defsubst spacemacs//org-snd-copy-if-asset (file raw-link path)
  ;; Errors:
  (cond
   ;; Missing target file.
   ((not (file-readable-p path))
    (spacemacs/org-sdn-error
     "File %S has a link to file %S but it isn't readable."
     file
     (file-truename path)))
   ;; Target file is outside documentation root.
   ((not (string-prefix-p spacemacs--root-dir
                          (file-truename path)))
    (spacemacs/org-sdn-error
     (concat "File %S has a link to file %S "
             "but it's outside of the documentation root directory.")
     file
     (file-truename path))))
  ;; Copy assets.
  (unless (string-match-p spacemacs--org-sdn-org-link-re raw-link)
    (spacemacs/org-sdn-export-file file (file-truename path))))


(defun spacemacs//org-sdn-link (link desc info)
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
                                  spacemacs--org-sdn-org-link-re
                                  raw-link)
                                 (match-string 1 raw-link))))
    (cond

     ;; Local .org file link with target(anchor).
     ((and file-path? org-link-with-target?)
      (spacemacs/org-sdn-error
       (concat "Link \"%s\" "
               "in \"%s\" "
               "should target the .org file at GitHub via web-link "
               "because it has target(anchors) and GitHub doesn't "
               "support them in ORG links :(\n"
               "(GitHub style anchors are supported)\n"
               "See footnote of %S for details.\n")
       raw-link
       file
       spacemacs-readme-template-url))

     ;; Web link to org file inside Spacemacs GitHub repository.
     ((string-match spacemacs--org-sdn-spacemacs-git-org-link-regexp raw-link)
      (let ((target-file (concat
                          spacemacs--root-dir
                          (url-unhex-string (match-string 1 raw-link)))))
        (unless (file-readable-p target-file)
          (spacemacs/org-sdn-error
           (concat
            "File %S has a GitHub link to a documentation file %S but "
            "it isn't readable locally.")
           file
           (file-truename target-file)))))

     ;; Link to a file.
     (file-path?
      (spacemacs//org-snd-copy-if-asset
       file
       raw-link
       (url-unhex-string path)))

     ;; Catch the rest of known types.
     ((member type '("http" "https" "custom-id" "ftp")))

     ;; Anything else is an error.
     (t (spacemacs/org-sdn-error
         (concat
          "Link \"%s\" in file \"%s\" "
          "has type \"%s\" "
          "but the type isn't implemented in spacemacs//org-sdn-link")
         raw-link
         file
         type)))
    (spacemacs//org-snd-fmt-output path type raw-link desc)))

;;;; Node Property

(defun spacemacs//org-sdn-node-property (_node-property _contents _info)
  "Transcode a NODE-PROPERTY element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-node-property")
  "")

;;;; Paragraph

(defun spacemacs//org-sdn-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element From Org to Spacemacs SDN.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (format "{:tag :paragraph :children [%s]}" contents))

;;;; Plain List

(defun spacemacs//org-sdn-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element From Org to Spacemacs SDN.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (parent-type
          (symbol-name
           (car
            (org-export-get-parent
             plain-list))))
         (parent-hl
          (org-export-get-parent-headline
           plain-list))
         (parent-hl-parent-hl
          (org-export-get-parent-headline
           parent-hl))
         (file (plist-get info :input-file)))
    (unless (or (eq 'ordered type)
                (eq 'unordered type)
                (eq 'descriptive type))
      (spacemacs/org-sdn-error
       (concat "File \"%s\" contains plain list of type \"%s\" but "
               "it isn't implemented in spacemacs//org-sdn-node-property")
       (plist-get info :input-file)
       type))
    (if (and (not
              ;; FIXME: We probably should use a better way to
              ;; tell apart nested features list and multiply
              ;; features list.
              (string= parent-type
                       "item"))
             (= (or (org-element-property
                     :level
                     parent-hl)
                    -1)
                2)
             (string= (org-element-property
                       :raw-value
                       parent-hl)
                      "Features:")
             (= (or (org-element-property
                     :level
                     parent-hl-parent-hl)
                    -1)
                1)
             (string= (org-element-property
                       :raw-value
                       parent-hl-parent-hl)
                      "Description"))
        (if (plist-member info :file-has-feature-list?)
            (spacemacs/org-sdn-error
             (concat "File \"%s\" has multiply "
                     "\"Features:\" lists in the top "
                     "level \"Description\" headline")
             file)
          (plist-put info :file-has-feature-list? 'true)
          (format "{:tag :feature-list :type :%s :children [%s]}"
                  type
                  contents))
      (format "{:tag :plain-list :type :%s :children [%s]}"
              type
              contents))))

;;;; Plain Text

(defun spacemacs//org-sdn-plain-text (text _info)
  "Transcode a TEXT string From Org to Spacemacs SDN.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (format "{:tag :plain-text :value \"%s\"}"
          (spacemacs/org-sdn-escape-string text)))

;;;; Planning

(defun spacemacs//org-sdn-planning (_planning _contents _info)
  "Transcode a PLANNING element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-planning")
  "")

;;;; Property Drawer

(defun spacemacs//org-sdn-property-drawer (_property-drawer _contents _info)
  "Transcode a PROPERTY-DRAWER element From Org to Spacemacs SDN.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-property-drawer")
  "")

;;;; Quote Block

(defun spacemacs//org-sdn-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "{:tag :quote :children [%s]}" contents))

;;;; Radio Target

(defun spacemacs//org-sdn-radio-target (_radio-target _text _info)
  "Transcode a RADIO-TARGET object From Org to Spacemacs SDN.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-radio-target")
  "")

;;;; Section

(defun spacemacs//org-sdn-section (_section contents _info)
  "Transcode a SECTION element From Org to Spacemacs SDN.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (format "{:tag :section :children [%s]}" contents))

;;;; Special Block

(defun spacemacs//org-sdn-special-block (_special-block _contents _info)
  "Transcode a SPECIAL-BLOCK element From Org to Spacemacs SDN.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-special-block")
  "")

;;;; Src Block

(defun spacemacs//org-sdn-src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element From Org to Spacemacs SDN.
CONTENTS is nil. INFO is a plist holding contextual information."
  (format "{:tag :src :language \"%s\" :value \"%s\"}"
          (spacemacs/org-sdn-escape-string
           (org-element-property :language src-block))
          (spacemacs/org-sdn-escape-string
           (org-element-property :value src-block))))

;;;; Statistics Cookie

(defun spacemacs//org-sdn-statistics-cookie (_statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-statistics-cookie")
  "")

;;;; Strike-Through

(defun spacemacs//org-sdn-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH From Org to Spacemacs SDN.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "{:tag :strike-through :children [%s]}" contents))

;;;; Subscript

(defun spacemacs//org-sdn-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object From Org to Spacemacs SDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "{:tag :subscript :children [%s]}" contents))

;;;; Superscript

(defun spacemacs//org-sdn-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object From Org to Spacemacs SDN.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "{:tag :superscript :children [%s]}" contents))

;;;; Table

(defun spacemacs//org-sdn-table (table contents _info)
  "Transcode a TABLE element From Org to Spacemacs SDN.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((type (org-element-property :type table)))
    (unless (eq type 'org)
      (spacemacs/org-sdn-error
       "Table type \"%s\" isn't implemented in spacemacs//org-sdn-table"
       type))
    (format "{:tag :table :type :%s :children [%s]}"
            (org-element-property :type table)
            contents)))

;;;; Table Cell

(defun spacemacs//org-sdn-table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "{:tag :table-cell :children [%s]}" contents))

;;;; Table Row

(defun spacemacs//org-sdn-table-row (table-row contents _info)
  "Transcode a TABLE-ROW element From Org to Spacemacs SDN.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (format "{:tag :table-row :type :%s :children [%s]}"
          (org-element-property :type table-row)
          contents))

;;;; Target

(defun spacemacs//org-sdn-target (_target _contents _info)
  "Transcode a TARGET object From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-target")
  "")

;;;; Template

(defun spacemacs//org-sdn-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((has-description?
         (plist-member info :file-has-description?))
        (has-feature-list?
         (plist-member info :file-has-feature-list?)))
    (when (plist-get info :input-file)
      (let ((file (file-truename
                   (plist-get info :input-file))))
        (when (and (string-prefix-p (file-truename
                                     (concat
                                      spacemacs--root-dir
                                      "layers/"))
                                    file)
                   (string-suffix-p "README.org"
                                    file
                                    t))
          (unless has-description?
            (spacemacs/org-sdn-error
             (concat
              "File \"%s\" "
              "doesn't have top level "
              "\"Description\" headline\n"
              "See %S\n")
             file
             spacemacs-readme-template-url))
          (unless has-feature-list?
            (spacemacs/org-sdn-error
             (concat "File \"%s\" "
                     "doesn't have \"Features:\"(With a colon) list in the "
                     "top level \"Description\" headline\n"
                     "See %S\n")
             file
             spacemacs-readme-template-url)))))
    (format (concat "{:tag :root "
                    ;; ":export-data #inst \"%s\" "
                    ":file-has-description? %s "
                    ":file-has-feature-list? %s "
                    ":headline-path-ids %s "
                    ":children [%s]}")
            ;; (format-time-string "%Y-%m-%dT%H:%M:%S.52Z" nil t)
            (if (plist-member info :file-has-description?)
                'true
              'false)
            (if (plist-member info :file-has-feature-list?)
                'true
              'false)
            (map 'vector
                 (lambda (s)
                   (format "\"%s\"" (spacemacs/org-sdn-escape-string s)))
                 (plist-get info :path-ids))
            contents)))

;;;; Timestamp

(defun spacemacs//org-sdn-timestamp (_timestamp _contents _info)
  "Transcode a TIMESTAMP object From Org to Spacemacs SDN.)))))))))
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (spacemacs/org-sdn-error "\"%s\" not implemented"
                           "spacemacs//org-sdn-timestamp")
  "")

;;;; Underline

(defun spacemacs//org-sdn-underline (_underline contents _info)
  "Transcode UNDERLINE From Org to Spacemacs SDN.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "{:tag :underline :children [%s]}" contents))

;;;; Verbatim

(defun spacemacs//org-sdn-verbatim (verbatim _contents _info)
  "Transcode VERBATIM From Org to Spacemacs SDN.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "{:tag :verbatim :value \"%s\"}"
          (spacemacs/org-sdn-escape-string
           (org-element-property :value verbatim))))

;;;; Verse Block

(defun spacemacs//org-sdn-verse-block (_verse-block contents _info)
  "Transcode a VERSE-BLOCK element From Org to Spacemacs SDN.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (format "{:tag :verse :children [%s]}" contents))


;;; Filter Functions

(defsubst spacemacs//org-sdn-final-function-tidy (contents)
  "Filter to compact output by removing newline symbols.
FIXME: Figure out where they come from :"
  (replace-regexp-in-string "\n" "" contents))

(defsubst spacemacs//org-sdn-final-function-fmt-vec-of-nil (str)
  "Replace [nil] with []."
  (replace-regexp-in-string "\\[nil\\]" "[]" str nil t))

(defsubst spacemacs//org-sdn-final-function-lint (info)
  "Warn about potential errors."
  (let ((warnings (plist-get info :spacemacs-sdn-warnings)))
    (when (stringp warnings)
      (spacemacs/org-sdn-warn
       "%s"
       (string-remove-suffix "\n" warnings)))))

(defun spacemacs//org-sdn-final-function (contents _backend info)
  "Call final functions for `space-sdn' backend"
  (spacemacs//org-sdn-final-function-lint info)
  (spacemacs//org-sdn-final-function-fmt-vec-of-nil
   (spacemacs//org-sdn-final-function-tidy contents)))


;;; End-user functions

(defun spacemacs/export-docs-to-sdn (root-dir exp-dir file-list)
  "Export org files in FILE-LIST into EXP-DIR.
ROOT-DIR is original documentation root directory."
  (let* ((spacemacs--root-dir (file-truename root-dir))
         (default-directory spacemacs--root-dir))
    (dolist (file file-list)
      (let* ((target-file-name (concat
                                exp-dir
                                (string-remove-suffix
                                 ".org"
                                 (string-remove-prefix
                                  spacemacs--root-dir
                                  (file-truename file)))
                                ".sdn"))
             (target-file-dir
              (file-name-as-directory
               (file-name-directory target-file-name))))
        ;; FIXME: Close enough. But it will be better if we
        ;; export stuff into separate directories and then merge.
        (while (not (file-accessible-directory-p target-file-dir))
          (condition-case err
              (make-directory target-file-dir t)
            (error (spacemacs/org-sdn-error
                    "make-directory \"%s\" failed with \"%s\". Retrying..."
                    target-file-dir
                    err))))
        (spacemacs/org-sdn-message
         "Exporting \"%s\" into \"%s\""
         file
         target-file-name)
        (with-temp-buffer
          (find-file file)
          (org-export-to-file
              'spacemacs-sdn
              target-file-name))
        (if (and (file-readable-p target-file-name)
                 (> (nth 7 (file-attributes target-file-name)) 0))
            (spacemacs/org-sdn-message
             "Successfully exported \"%s\""
             file)
          (spacemacs/org-sdn-error
           "Export finished but \"%s\" doesn't exist or empty"
           target-file-name))))))

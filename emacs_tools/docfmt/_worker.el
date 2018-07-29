;;; _worker.el --- Spacemacs doc formatter worker -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;         Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
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
  (require 'cl))

(defconst docfmt-lib-dir
  (file-truename
   (concat
    (file-name-directory
     (or load-file-name
         buffer-file-name))
    "../lib/")))

(load (concat docfmt-lib-dir "toc-org.elc")  nil t)

(defconst docfmt-title-regexp "^#\\+TITLE:.*$")
(defconst docfmt-begin-block-regexp "^#\\+BEGIN.*$")
(defconst docfmt-end-block-regexp "^#\\+END.*$")
(defconst docfmt-empty-line-regexp "^[ \t]*$")
(defconst docfmt-tree-trunk-regexp "^[ 	]*|_")
(defconst docfmt-toc-heading-head "* Table of Contents")
(defconst docfmt-toc-heading-tail ":TOC_4_gh:noexport:")
(defconst docfmt-toc-headline (format
                               "%-41s%s"
                               docfmt-toc-heading-head
                               docfmt-toc-heading-tail))

(defsubst docfmt/rm-empty-lines-at-beg ()
  "Remove newlines at the beginning of the buffer."
  (goto-char (point-min))
  (while (looking-at-p docfmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst docfmt/rm-empty-lines-at-end ()
  "Remove newlines at the ending of the buffer."
  (goto-char (point-max))
  (delete-blank-lines)
  (delete-blank-lines))

(defsubst docfmt/rm-trail-delim-in-hl ()
  "Remove trailing delimiters in headlines."
  (goto-char (point-min))
  (while (re-search-forward "^*+[[:space:]]+.*\\([;,\\.[:space:]]+\\)$" nil t)
    (replace-match "" nil nil nil 1)
    (forward-line -1)))

(defsubst docfmt/remove-readtheorg-meta ()
  "Remove '#+HTML_HEAD_EXTRA: ... readtheorg.css\" />'."
  (goto-char (point-min))
  (while (re-search-forward "#\\+HTML_HEAD_EXTRA.*readtheorg\\\.css.*" nil t)
    (replace-match "")))

(defsubst docfmt/multy-nl-with-single ()
  "Replace multiply empty lines with a single empty line."
  (goto-char (point-min))
  (while (re-search-forward "\\(^[[:space:]]*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char)))

(defsubst docfmt/replace-org-toc ()
  "Replace \":TOC_X_org:\" with \":TOC_4_gh:\"."
  (goto-char (point-min))
  (while (re-search-forward toc-org-toc-org-regexp nil t)
    (replace-match "_4_gh" nil nil nil 2)))

(defsubst docfmt/goto-next-table ()
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
   (unless (looking-at-p docfmt-tree-trunk-regexp)
     (return)))
  (looking-at-p org-table-any-line-regexp))

(defsubst docfmt/remote-empty-lines-at-the-beginning ()
  "Remove empty lines at the begging of the buffer."
  (goto-char (point-min))
  (while (looking-at-p docfmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst docfmt/insert-empty-line-after-title ()
  "Insert an empty line after title."
  (goto-char (point-min))
  (when (looking-at-p docfmt-title-regexp)
    (forward-line 1)
    (unless (looking-at-p docfmt-empty-line-regexp)
      (open-line 1))))

(defsubst docfmt/insert-empty-line-before-begin-block ()
  "Insert an empty line before begins of blocks."
  (goto-char (point-max))
  (while (re-search-backward docfmt-begin-block-regexp nil t)
    (goto-char (point-at-bol))
    (forward-line -1)
    (unless (or (looking-at-p docfmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (forward-line 1)
      (open-line 1))))

(defsubst docfmt/insert-empty-line-after-end-block ()
  "Insert an empty line after ends of blocks."
  (goto-char (point-min))
  (while (re-search-forward docfmt-end-block-regexp nil t)
    (forward-line 1)
    (unless (looking-at-p docfmt-empty-line-regexp)
      (open-line 1))))

(defsubst docfmt/insert-empty-line-at-the-end ()
  "Insert an empty line at the end of the buffer."
  (goto-char (point-max))
  (unless (looking-at-p docfmt-empty-line-regexp)
    (open-line 1)))

(defsubst docfmt/insert-title ()
  "Insert #TITLE:{DIR_NAME} if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (looking-at-p docfmt-title-regexp)
    (insert (format "#+TITLE:%s\n"
                    (file-name-base
                     (directory-file-name
                      (file-name-directory
                       (buffer-file-name))))))))

(defsubst docfmt/insert-toc ()
  "Insert toc if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (re-search-forward toc-org-toc-org-regexp nil t)
    (goto-char (point-max))
    ;; Skip from the end of the buffer to the first headling.
    (while (re-search-backward org-heading-regexp nil t))
    (open-line 3)
    (forward-line 1)
    (insert docfmt-toc-headline)))

(defsubst docfmt/remove-empty-lines-after-headlines()
  "Remove empty liners after each headline."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (unless (= (forward-line) 0)
      (while (looking-at-p docfmt-empty-line-regexp)
        (delete-blank-lines)))))

(defsubst docfmt/insert-empty-line-before-tables ()
  "Insert an empty line before each org table."
  (goto-char (point-min))
  (while (docfmt/goto-next-table)
    (forward-line -1)
    (unless (looking-at-p docfmt-empty-line-regexp)
      (end-of-line)
      (open-line 1))
    (forward-line 1)))

(defsubst docfmt/insert-empty-line-after-sections ()
  "Insert an empty line after each section."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (forward-line -1)
    (unless (or (looking-at-p docfmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (end-of-line)
      (open-line 1))
    (forward-line 2)))

(defsubst docfmt/insert-empty-line-after-tables ()
  "Insert an empty line after each table."
  (goto-char (point-min))
  (while (docfmt/goto-next-table)
    ;; Skip current table.
    (while (looking-at-p org-table-any-line-regexp)
      (forward-line))
    (unless (looking-at-p docfmt-empty-line-regexp)
      (goto-char (point-at-bol))
      (open-line 1)
      (forward-line))))

(defsubst docfmt/align-tables ()
  "Align all tables"
  (goto-char (point-min))
  (while (docfmt/goto-next-table)
    (ignore-errors
      (org-table-align))))

(defsubst docfmt/apply-toc ()
  "Apply current toc-org TAG to TOC."
  (toc-org-enable)
  (goto-char (point-min))
  (toc-org-insert-toc))

(defun docfmt/apply-all ()
  "Format current `org-mode' buffer."
  (let ((old-buff-str (buffer-string))
        (new-buff-str ""))
    (cl-loop
     (docfmt/rm-empty-lines-at-beg)
     (docfmt/rm-empty-lines-at-end)
     (docfmt/remove-readtheorg-meta)
     (docfmt/replace-org-toc)
     (docfmt/rm-trail-delim-in-hl)
     (docfmt/multy-nl-with-single)
     (docfmt/remote-empty-lines-at-the-beginning)
     (docfmt/insert-title)
     (docfmt/insert-toc)
     (docfmt/apply-toc)
     (docfmt/remove-empty-lines-after-headlines)
     (docfmt/insert-empty-line-before-tables)
     (docfmt/insert-empty-line-after-title)
     (docfmt/insert-empty-line-after-tables)
     (docfmt/insert-empty-line-after-sections)
     (docfmt/insert-empty-line-before-begin-block)
     (docfmt/insert-empty-line-after-end-block)
     (docfmt/insert-empty-line-at-the-end)
     (docfmt/align-tables)
     (setq new-buff-str (buffer-string))
     (if (string= old-buff-str new-buff-str)
         (return)
       (setq old-buff-str new-buff-str)))))

(defun docfmt/apply-all-batch (files)
  "Function for batch processing FILES in `noninteractive' mode."
  (message "Files to be formatted: %S" files)
  (dolist (file files)
    (with-temp-file file
      (insert-file-contents file)
      (set-visited-file-name file t t)
      (docfmt/apply-all)))
  (message "Done."))

;;; _worker.el --- Spacemacs docs prefmt worker -*- lexical-binding: t -*-
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

(require 'org)

(eval-when-compile
  (require 'cl))

(defconst prefmt-title-regexp "^#\\+TITLE:.*$")
(defconst prefmt-begin-block-regexp "^#\\+BEGIN.*$")
(defconst prefmt-end-block-regexp "^#\\+END.*$")
(defconst prefmt-empty-line-regexp "^[ \t]*$")
(defconst prefmt-tree-trunk-regexp "^[ 	]*|_")

(defsubst prefmt/rm-empty-lines-at-beg ()
  "Remove newlines at the beginning of the buffer."
  (goto-char (point-min))
  (while (looking-at-p prefmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst prefmt/rm-empty-lines-at-end ()
  "Remove newlines at the ending of the buffer."
  (goto-char (point-max))
  (delete-blank-lines)
  (delete-blank-lines))

(defsubst prefmt/rm-trail-delim-in-hl ()
  "Remove trailing delimiters in headlines."
  (goto-char (point-min))
  (while (re-search-forward "^*+[[:space:]]+.*\\([;,\\.[:space:]]+\\)$" nil t)
    (replace-match "" nil nil nil 1)
    (forward-line -1)))

(defsubst prefmt/multy-nl-with-single ()
  "Replace multiply empty lines with a single empty line."
  (goto-char (point-min))
  (while (re-search-forward "\\(^[[:space:]]*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char)))

(defsubst prefmt/goto-next-table ()
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
   (unless (looking-at-p prefmt-tree-trunk-regexp)
     (return)))
  (looking-at-p org-table-any-line-regexp))

(defsubst prefmt/remote-empty-lines-at-the-beginning ()
  "Remove empty lines at the begging of the buffer."
  (goto-char (point-min))
  (while (looking-at-p prefmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst prefmt/insert-empty-line-after-title ()
  "Insert an empty line after title."
  (goto-char (point-min))
  (when (looking-at-p prefmt-title-regexp)
    (forward-line 1)
    (unless (looking-at-p prefmt-empty-line-regexp)
      (open-line 1))))

(defsubst prefmt/insert-empty-line-before-begin-block ()
  "Insert an empty line before begins of blocks."
  (goto-char (point-max))
  (while (re-search-backward prefmt-begin-block-regexp nil t)
    (goto-char (point-at-bol))
    (forward-line -1)
    (unless (or (looking-at-p prefmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (forward-line 1)
      (open-line 1))))

(defsubst prefmt/insert-empty-line-after-end-block ()
  "Insert an empty line after ends of blocks."
  (goto-char (point-min))
  (while (re-search-forward prefmt-end-block-regexp nil t)
    (forward-line 1)
    (unless (looking-at-p prefmt-empty-line-regexp)
      (open-line 1))))

(defsubst prefmt/insert-empty-line-at-the-end ()
  "Insert an empty line at the end of the buffer."
  (goto-char (point-max))
  (unless (looking-at-p prefmt-empty-line-regexp)
    (open-line 1)))

(defsubst prefmt/insert-title ()
  "Insert #TITLE:{DIR_NAME} if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (looking-at-p prefmt-title-regexp)
    (insert (format "#+TITLE:%s\n"
                    (file-name-base
                     (directory-file-name
                      (file-name-directory
                       (buffer-file-name))))))))

(defsubst prefmt/remove-empty-lines-after-headlines()
  "Remove empty liners after each headline."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (unless (= (forward-line) 0)
      (while (looking-at-p prefmt-empty-line-regexp)
        (delete-blank-lines)))))

(defsubst prefmt/insert-empty-line-before-tables ()
  "Insert an empty line before each org table."
  (goto-char (point-min))
  (while (prefmt/goto-next-table)
    (forward-line -1)
    (unless (looking-at-p prefmt-empty-line-regexp)
      (end-of-line)
      (open-line 1))
    (forward-line 1)))

(defsubst prefmt/insert-empty-line-after-sections ()
  "Insert an empty line after each section."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (forward-line -1)
    (unless (or (looking-at-p prefmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (end-of-line)
      (open-line 1))
    (forward-line 2)))

(defsubst prefmt/insert-empty-line-after-tables ()
  "Insert an empty line after each table."
  (goto-char (point-min))
  (while (prefmt/goto-next-table)
    ;; Skip current table.
    (while (looking-at-p org-table-any-line-regexp)
      (forward-line))
    (unless (looking-at-p prefmt-empty-line-regexp)
      (goto-char (point-at-bol))
      (open-line 1)
      (forward-line))))

(defsubst prefmt/align-tables ()
  "Align all tables"
  (goto-char (point-min))
  (while (prefmt/goto-next-table)
    (ignore-errors
      (org-table-align))))

(defun prefmt/apply-all ()
  "Format current `org-mode' buffer."
  (let ((old-buff-str (buffer-string))
        (new-buff-str ""))
    (cl-loop
     (prefmt/rm-empty-lines-at-beg)
     (prefmt/rm-empty-lines-at-end)
     (prefmt/rm-trail-delim-in-hl)
     (prefmt/multy-nl-with-single)
     (prefmt/remote-empty-lines-at-the-beginning)
     (prefmt/insert-title)
     (prefmt/remove-empty-lines-after-headlines)
     (prefmt/insert-empty-line-before-tables)
     (prefmt/insert-empty-line-after-title)
     (prefmt/insert-empty-line-after-tables)
     (prefmt/insert-empty-line-after-sections)
     (prefmt/insert-empty-line-before-begin-block)
     (prefmt/insert-empty-line-after-end-block)
     (prefmt/insert-empty-line-at-the-end)
     (prefmt/align-tables)
     (setq new-buff-str (buffer-string))
     (if (string= old-buff-str new-buff-str)
         (return)
       (setq old-buff-str new-buff-str)))))

(defun prefmt/apply-all-batch (files)
  "Function for batch processing FILES in `noninteractive' mode."
  (message "Files to be formatted: %S" files)
  (dolist (file files)
    (with-temp-file file
      (insert-file-contents file)
      (set-visited-file-name file t t)
      (prefmt/apply-all)))
  (message "Done."))

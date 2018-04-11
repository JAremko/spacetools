#!/usr/bin/emacs --script
;;
;;; run.el -- Spacemacs docs formatter runner -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;; Note: see `spacemacs-docfmt-help-text' for usage.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(defconst spacemacs-docfmt-help-text
  (concat
   "Spacemacs documentation formatting tool\n"
   "=======================================\n"
   "Arguments are *.org file paths or directories.\n"
   "If a file path is a directory - it will be searched\n"
   "for *.org files.")
  "Help text for the script.")

(declare-function spacemacs/docfmt-apply-all "_worker.el" nil)
(declare-function spacemacs//spacetools-get-cpu-count "shared.el" nil)
(declare-function spacemacs//spacetools-find-org-files "shared.el" (paths))
(declare-function spacemacs/docfmt-apply-all-batch "_worker.el" (files))
(declare-function spacemacs//spacetools-do-concurrently "shared.el" (files
                                                                     w-count
                                                                     w-path
                                                                     make-task
                                                                     sentinel))

(defconst spacemacs-docfmt-run-file-name (or load-file-name buffer-file-name)
  "Path to run script of \"docfmt\" tool.")

(defconst spacemacs-docfmt-run-file-dir
  (file-name-directory spacemacs-docfmt-run-file-name)
  "Path to \"docfmt\" tool directory.")

(defconst spacemacs--docfmt-worker-el-path
  (concat spacemacs-docfmt-run-file-dir "_worker.el")
  "Path to worker script .el file")

(defconst spacemacs--docfmt-worker-path
  (concat spacemacs-docfmt-run-file-dir "_worker.elc")
  "Path to compiled worker script file.")

(defconst spacemacs--docfmt-shared-path
  (expand-file-name "../lib/shared.el" spacemacs-docfmt-run-file-dir)
  "Path to shared lib.")

(defvar spacemacs--docfmt-workers-count nil
  "Number of Emacs instances that will be used for formatting.")
(defvar spacemacs--docfmt-workers-fin 0
  "Number of workers finished.")
(defvar spacemacs--docfmt-stop-waiting nil
  "Used for blocking until all formatters have exited.")

(load spacemacs--docfmt-shared-path nil t)

(defun spacemacs/docfmt-format ()
  "Format current `org-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode major mode should be enabled in the file."))
  (spacemacs/docfmt-apply-all))

(defun spacemacs//docfmt-concurrently-sentinel (p e)
  (condition-case err
      (let ((buff (process-buffer p)))
        (if (not (eq (process-status p) 'exit))
            (error "Process %s doesn't have status: exit" p)
          (dolist (line (split-string (with-current-buffer buff (buffer-string))
                                      "\n"))
            (unless (or (string= line "")
                        (string-match-p "^Loading.*\\.\\.\\.$" line))
              (message "Process \"%s\" says:" p)
              (message line)))
          (kill-buffer buff))
        (if (string-match-p "finished" e)
            (progn
              (message "Process \"%s\" has finished\n" p)
              (when
                  (= (setq spacemacs--docfmt-workers-fin
                           (1+ spacemacs--docfmt-workers-fin))
                     spacemacs--docfmt-workers-count)
                (setq spacemacs--docfmt-stop-waiting t)))
          (error "Process %s was %s"
                 p e)
          (setq spacemacs--docfmt-stop-waiting t)))
    (error (setq spacemacs--docfmt-stop-waiting t)
           (error "%s" err))))

(defun spacemacs/docfmt-run (arg-list)
  "Main function for running as a script.
ARG-LIST is an argument list where the fist element is the number of emacs
process that will be used and
the rest elements are file paths (absolute or relative to Spacemacs root dir)."
  (when (or (not arg-list)
            (member (car arg-list)
                    '("-h" "help")))
    (error spacemacs-docfmt-help-text))
  (setq spacemacs--docfmt-workers-fin 0
        spacemacs--docfmt-stop-waiting nil)
  (let* ((files (spacemacs//spacetools-find-org-files arg-list))
         (f-length (length files))
         (w-count (min (spacemacs//spacetools-get-cpu-count) f-length))
         (toc-org-fp-pref (expand-file-name
                           "../lib/toc-org"
                           spacemacs-docfmt-run-file-dir)))
    (when (> f-length 0)
      (byte-compile-file (concat toc-org-fp-pref ".el"))
      ;; We doing it this way to suppress "Loading" message.
      (load (concat toc-org-fp-pref ".elc") nil t)
      (byte-compile-file spacemacs--docfmt-worker-el-path)
      (if (= w-count 1)
          (progn
            (load spacemacs--docfmt-worker-path nil t)
            (spacemacs/docfmt-apply-all-batch files))
        (spacemacs//spacetools-do-concurrently
         files
         (setq spacemacs--docfmt-workers-count w-count)
         spacemacs--docfmt-worker-path
         (lambda (fps) (format "%S" `(spacemacs/docfmt-apply-all-batch ',fps)))
         'spacemacs//docfmt-concurrently-sentinel)
        (while (not spacemacs--docfmt-stop-waiting)
          (accept-process-output))))))

;; Script entry point.
(when (and load-file-name noninteractive)
  (spacemacs/docfmt-run argv))

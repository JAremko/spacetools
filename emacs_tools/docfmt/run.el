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
;; Note: see `docfmt-help-text' for usage.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(defconst docfmt-help-text
  (concat
   "Spacemacs documentation formatting tool\n"
   "=======================================\n"
   "Arguments are *.org file paths or directories.\n"
   "If a file path is a directory - it will be searched\n"
   "for *.org files.")
  "Help text for the script.")

(declare-function docfmt/apply-all "_worker.el" nil)
(declare-function spacetools/get-cpu-count "shared.el" nil)
(declare-function spacetools/find-org-files "shared.el" (paths))
(declare-function docfmt/apply-all-batch "_worker.el" (files))
(declare-function spacetools/do-concurrently "shared.el" (files
                                                          w-count
                                                          w-path
                                                          sentinel
                                                          make-task))

(defconst docfmt-run-file-name (or load-file-name buffer-file-name)
  "Path to run script of \"docfmt\" tool.")

(defconst docfmt-run-file-dir
  (file-name-directory docfmt-run-file-name)
  "Path to \"docfmt\" tool directory.")

(defconst docfmt-worker-el-path
  (concat docfmt-run-file-dir "_worker.el")
  "Path to worker script .el file")

(defconst docfmt-worker-path
  (concat docfmt-run-file-dir "_worker.elc")
  "Path to compiled worker script file.")

(defconst docfmt-shared-path
  (expand-file-name "../lib/shared.el" docfmt-run-file-dir)
  "Path to shared lib.")

(defvar docfmt-workers-count nil
  "Number of Emacs instances that will be used for formatting.")
(defvar docfmt-workers-fin 0
  "Number of workers finished.")
(defvar docfmt-stop-waiting nil
  "Used for blocking until all formatters have exited.")

(load docfmt-shared-path nil t)

(defun docfmt/format ()
  "Format current `org-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode major mode should be enabled in the file."))
  (docfmt/apply-all))

(defun docfmt/concurrently-sentinel (p e)
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
                  (= (setq docfmt-workers-fin
                           (1+ docfmt-workers-fin))
                     docfmt-workers-count)
                (setq docfmt-stop-waiting t)))
          (error "Process %s was %s"
                 p e)
          (setq docfmt-stop-waiting t)))
    (error (setq docfmt-stop-waiting t)
           (error "%s" err))))

(defun docfmt/run (arg-list)
  "Main function for running as a script.
ARG-LIST is an argument list where the fist element is the number of emacs
process that will be used and
the rest elements are file paths (absolute or relative to Spacemacs root dir)."
  (when (or (not arg-list)
            (member (car arg-list)
                    '("-h" "help")))
    (error docfmt-help-text))
  (setq docfmt-workers-fin 0
        docfmt-stop-waiting nil)
  (let* ((files (spacetools/find-org-files arg-list))
         (f-length (length files))
         (w-count (min (spacetools/get-cpu-count) f-length))
         (toc-org-fp-pref (expand-file-name
                           "../lib/toc-org"
                           docfmt-run-file-dir)))
    (if (= f-length 0)
        (progn
          (message "No files to format.")
          (kill-emacs 0))
      (byte-compile-file (concat toc-org-fp-pref ".el"))
      ;; We doing it this way to suppress "Loading" message.
      (load (concat toc-org-fp-pref ".elc") nil t)
      (byte-compile-file docfmt-worker-el-path)
      (if (= w-count 1)
          (progn
            (load docfmt-worker-path nil t)
            (docfmt/apply-all-batch files))
        (spacetools/do-concurrently
         files
         (setq docfmt-workers-count w-count)
         docfmt-worker-path
         'docfmt/concurrently-sentinel
         (lambda (f) (format "%S" `(docfmt/apply-all-batch ',f))))
        (while (not docfmt-stop-waiting)
          (accept-process-output))))))

;; Script entry point.
(when (and load-file-name noninteractive)
  (docfmt/run argv))

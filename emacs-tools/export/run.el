#!/usr/bin/emacs --script
;;
;;; run.el -- Spacemacs documentation export runner -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;; Note: see `spacemacs-export-docs-help-text' for usage.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defconst spacemacs-export-docs-help-text
  (concat
   "Spacemacs documentation formatting tool\n"
   "=======================================\n"
   "First argument is the root directory (usually ~/.emacs.d/).\n"
   "It will be used to transform paths.\n"
   "The rest of arguments are \"*.org\" file paths or directories.\n"
   "Directories will be searched for *.org files.\n"
   "Files will be exported into \"export/target\" directory of the tool.\n"
   "Script can be called only with the first argument. In this case\n"
   "a default list of Spacemacs documentation files will be used.")
  "Help text for the script.")

(declare-function spacemacs//spacetools-find-org-files "shared.el" (paths))
(declare-function spacemacs//spacetools-get-cpu-count "shared.el" nil)
(declare-function spacemacs//spacetools-do-concurrently "shared.el" (files
                                                                     w-count
                                                                     w-path
                                                                     make-task
                                                                     sentinel))

(defconst spacemacs-export-docs-run-file-name
  (or load-file-name buffer-file-name)
  "Path to  run script of \"export\" tool.")

(defconst spacemacs-export-docs-run-file-dir
  (file-name-directory spacemacs-export-docs-run-file-name)
  "Path to \"export\" tool directory.")

(defconst spacemacs-export-docs-target-dir
  (concat spacemacs-export-docs-run-file-dir "target/")
  "Target directory for \"export\" tool.")

(load
 (expand-file-name
  "../lib/shared.el"
  spacemacs-export-docs-run-file-dir)
 nil t)
(defvar spacemacs--export-docs-root-dir ""
  "Root directory of the original documentation.")
(defvar spacemacs--export-docs-workers-fin 0
  "Number of Emacs instances that finished exporting.")
(defvar spacemacs--export-docs-stop-waiting nil
  "Used for blocking until all exporters have exited.")
(defvar spacemacs--export-docs-worker-count 0
  "How many workers(Emacs instances) should we use for exporting.")
(defvar spacemacs--export-docs-worker-errored? nil
  "Will be set to last error message if a worker emitted a standard errors.")
(defvar spacemacs--export-docs-copy-queue '()
  "Queue of static dependencies to be copied to
the export dir.")
(defvar spacemacs--export-docs-default-exclude
  `("export/"
    "private/"
    "tests/"
    "elpa/"
    "layers/LAYERS.org")
  "List of Spacemacs directories and ORG files that normally
 shouldn't be exported.")



(defun spacemacs//export-docs-copy-file-to-target-dir (file-path)
  "Copy file at FILE-PATH into `spacemacs-export-docs-target-dir'.
ROOT-DIR is the documentation root directory. Empty FILE-PATH ignored."
  (unless (string-empty-p file-path)
    (let* ((op (file-relative-name file-path spacemacs--export-docs-root-dir))
           (np (expand-file-name op spacemacs-export-docs-target-dir))
           (np-dir (file-name-directory np)))
      (make-directory np-dir t)
      (message "Copying file %S into %S" file-path np)
      (copy-file file-path np t))))

(defun spacemacs//export-docs-sentinel (p e)
  (condition-case err
      (let ((buff (process-buffer p)))
        (if (not (eq (process-status p) 'exit))
            (error "Process %s doesn't have status: exit" p)
          (spacemacs//export-docs-interpret-proc-output p buff)
          (kill-buffer buff))
        (if (string-match-p "finished" e)
            (progn
              (message "Process %s has finished\n" p)
              (when (= (cl-incf spacemacs--export-docs-workers-fin)
                       spacemacs--export-docs-worker-count)
                (setq spacemacs--export-docs-stop-waiting t)))
          (error "Process %s was %s"
                 p e)
          (setq spacemacs--export-docs-stop-waiting t)))
    (error (setq spacemacs--export-docs-stop-waiting t)
           (error "%s" err))))

(defun spacemacs//export-docs-interpret-proc-output (proc buff)
  "Parses process PROC BUFFER. Process P should be finished."
  (message "PROCESS: %S\n" proc)
  (dolist (line (split-string (with-current-buffer buff (buffer-string)) "\n"))
    (unless (or (string= line "")
                (string-match-p "^Loading.*\\.\\.\\.$" line))
      (let ((resp (ignore-errors (json-read-from-string line))))
        (unless resp
          (error "Malformed response:%s" line))
        (let ((type (alist-get 'type resp))
              (text (replace-regexp-in-string
                     "\r"
                     "\n"
                     (alist-get 'text resp))))
          (message
           "%s"
           (cond
            ((string= type "message")
             text)
            ((string= type "warning")
             (concat "\n=============== WARNING ===============\n"
                     text
                     "\n=======================================\n"))
            ((string= type "error")
             (setq spacemacs--export-docs-worker-errored?
                   (concat "\n!!!!!!!!!!!!!!!! ERROR !!!!!!!!!!!!!!!!\n"
                           text
                           "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")))
            ((string= type "export")
             (format
              (concat "File %S has static dependency %S\n"
                      "=> it will be copied into the export directory")
              (alist-get 'source resp)
              (progn
                (push text spacemacs--export-docs-copy-queue)
                text)))
            (t
             (error
              "%s"
              (concat "\n?????????? UNKNOWN EVENT TYPE ????????????\n"
                      (format "TYPE:\"%s\" TEXT: \"%s\"" type text)
                      "\n?????????????????????????????????????????\n")))))))))
  (while spacemacs--export-docs-copy-queue
    (spacemacs//export-docs-copy-file-to-target-dir
     (pop spacemacs--export-docs-copy-queue))))

(defun spacemacs//export-docs-build-default-list (root-dir)
  "Create default list of Spacemacs documentation files to export."
  (let ((exclude-re (regexp-opt (mapcar
                                 (apply-partially 'concat root-dir)
                                 spacemacs--export-docs-default-exclude))))
    (delete 0 (mapcar (lambda (path) (or (string-match-p exclude-re path) path))
                      (directory-files-recursively root-dir "\\.org$")))))

(defun spacemacs//export-docs-run (arg-list)
  "Main function for running as a script. ARG-LIST is an argument list.
See `spacemacs-export-docs-help-text' for description."
  (unless arg-list
    (error spacemacs-export-docs-help-text))
  (unless (file-directory-p (car arg-list))
    (error "The first argument must be a readable directory."))
  (setq spacemacs--export-docs-workers-fin 0
        spacemacs--export-docs-stop-waiting nil)
  (let* ((default-directory spacemacs-export-docs-run-file-dir)
         (w-path (progn (byte-compile-file "_worker.el")
                        (file-truename "_worker.elc")))
         (root-dir (file-truename (file-name-as-directory (pop arg-list))))
         (files (let ((default-directory root-dir))
                  (spacemacs//spacetools-find-org-files
                   (or arg-list
                       (spacemacs//export-docs-build-default-list root-dir)))))
         (f-length (length files))
         (w-count
          ;; FIXME: With 1-2  workers it gets extremely slow.
          ;; Find the bottleneck.
          ;;(min (spacemacs//spacetools-get-cpu-count) f-length)
          16))
    (setq spacemacs--export-docs-worker-count w-count
          spacemacs--export-docs-root-dir root-dir)
    (spacemacs//spacetools-do-concurrently
     files
     w-count
     w-path
     (lambda (fps)
       (format
        "%S"
        `(spacemacs/export-docs-to-sdn
          ,root-dir
          ,spacemacs-export-docs-target-dir
          ',fps)))
     'spacemacs//export-docs-sentinel)
    (while (not spacemacs--export-docs-stop-waiting)
      (accept-process-output))))

;; Script entry point.
(when (and load-file-name noninteractive)
  (spacemacs//export-docs-run argv))

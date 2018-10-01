#!/usr/bin/emacs --script
;;
;;; docfmt.el -- Spacemacs docs formatter runner -*- lexical-binding: t -*-
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
   "for *.org files.\n"
   "Files are formatted in-place.")
  "Help text for the script.")

(declare-function docfmt/apply-all "_worker.el" nil)
(declare-function docfmt/apply-all-batch "_worker.el" (files))

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

(defvar docfmt-workers-count nil
  "Number of Emacs instances that will be used for formatting.")
(defvar docfmt-workers-fin 0
  "Number of workers finished.")
(defvar docfmt-stop-waiting nil
  "Used for blocking until all formatters have exited.")

;;; NOTE: Mb move back to the shared file?
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun docfmt/make-file-size-path-alist (files)
  "Return (<file size> . <abs file path>) alist of FILES."
  (let ((res nil))
    (dolist (file files)
      (unless (and (file-readable-p file)
                   (not (file-directory-p file)))
        (error "File \"%s\" unwritable or directory"
               file))
      (push (cons (float (nth 7 (file-attributes file)))
                  (expand-file-name file))
            res))
    res))

(defun docfmt/get-cpu-count ()
  "Get number of processor cores or return \"8\" :P"
  (let
      ((res (or (let ((win-cpu-num (getenv "NUMBER_OF_PROCESSORS")))
                  (when win-cpu-num (string-to-number win-cpu-num)))
                (with-temp-buffer
                  (ignore-errors
                    (when (zerop
                           (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
                      (string-to-number (buffer-string)))))
                (when (file-exists-p "/proc/cpuinfo")
                  (with-temp-buffer
                    (insert-file-contents "/proc/cpuinfo")
                    (how-many "^processor[[:space:]]+:"))))))
    (if (and (numberp res)
             (> res 0))
        res
      ;; Fallback value.
      8)))

(defun docfmt/find-org-files (paths)
  "Build list of absolute paths to org files based of PATHS.
Each path must be path to an org file or a directory.
If it is directory find all org files in it and append
to the return value."
  (let ((ret '()))
    (mapcar
     (lambda (path)
       (let ((p (file-truename path)))
         (if (file-exists-p p)
             (if (file-directory-p p)
                 (dolist (fp (directory-files-recursively p "\\.org$"))
                   (push fp ret))
               (push p ret))
           (error "File: \"%s\" doesn't exist or unreadable." p))))
     paths)
    ret))

(defun docfmt/files-to-buckets (files n)
  "Split FILES into N lists(task buckets) balancing by file sizes."
  (let ((fps-alist (sort
                    (docfmt/make-file-size-path-alist
                     files)
                    (lambda (e1 e2) (> (car e1) (car e2)))))
        (buckets '()))
    (dotimes (_ n) (push (cl-copy-list '(0)) buckets))
    (dolist (fps fps-alist)
      (setf buckets (sort
                     buckets
                     (lambda (e1 e2) (< (car e1) (car e2))))
            (car buckets)
            (cons (+ (caar buckets) (car fps))
                  (push (cdr fps) (cdar buckets)))))
    (mapcar 'cdr buckets)))
(byte-compile 'docfmt-filse-to-buckets)

(defun docfmt/do-concurrently
    (files w-count w-path sentinel make-task)
  "Run task concurrently.
Process FILES using W-COUNT workers(child emacs processes) loaded from W-PATH
MAKE-TASK is a function that takes single argument (file list) and returns
string representation of a task that each worker will perform - it will be
called as \"emacs -l W-PATH --batch -eval <value returned from MAKE-TASK>\"/.
SENTINEL is a worker process sentinel."
  (let ((file-buckets '())
        (emacs-fp (executable-find "emacs")))
    (unless emacs-fp
      (error "Can't find emacs executable"))
    (setq file-buckets
          (docfmt/files-to-buckets
           files
           (min w-count
                (length files))))
    (dolist (file-path-bucket file-buckets)
      (make-process
       :name
       "worker"
       :sentinel
       sentinel
       :buffer
       (generate-new-buffer "workers")
       :command
       (list emacs-fp
             "-Q"
             "-l" w-path
             "--batch"
             "-eval" (funcall
                      make-task
                      file-path-bucket))))))
;; -----------------------------------------------------------------------------

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
  (let* ((files (docfmt/find-org-files arg-list))
         (f-length (length files))
         (w-count (min (docfmt/get-cpu-count) f-length)))
    (if (= f-length 0)
        (progn
          (message "No files to format.")
          (kill-emacs 0))
      (byte-compile-file docfmt-worker-el-path)
      (if (= w-count 1)
          (progn
            (load docfmt-worker-path nil t)
            (docfmt/apply-all-batch files))
        (docfmt/do-concurrently
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

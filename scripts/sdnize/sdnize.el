#!/usr/bin/emacs --script
;;
;;; sdnize.el -- Spacemacs documentation SDN exporter -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;; Note: see `sdnize-help-text' for usage.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defconst sdnize-help-text
  (concat
   "Spacemacs documentation formatting tool\n"
   "=======================================\n"
   "Usage: sdnize.el [options] arguments..\n"
   "\n"
   "Options:\n"
   "+copy-assets - copy files referred in the exported .org files.\n"
   "\n"
   "Arguments:\n"
   "First argument is the root directory (usually ~/.emacs.d/).\n"
   "It will be used to transform paths.\n"
   "Second argument is the target directory.\n"
   "The rest of arguments are \"*.org\" file paths or directories.\n"
   "Directories will be searched for *.org files.\n"
   "Script can be called only with the first two argument. In this case\n"
   "a default list of Spacemacs documentation files will be used.")
  "Help text for the script.")

(defconst sdnize-run-file-name
  (or load-file-name buffer-file-name)
  "Path to this script.")

(defconst sdnize-run-file-dir
  (file-name-directory sdnize-run-file-name)
  "Path to the parent directory of this file.")

(defvar sdnize-copy-assets nil
  "If not-nil the script will copy files referred in the exported .org files.")

(defvar sdnize-target-dir ""
  "Target directory.")

(defvar sdnize-root-dir ""
  "Root directory of the original documentation.")
(defvar sdnize-workers-fin 0
  "Number of Emacs instances that finished exporting.")
(defvar sdnize-stop-waiting nil
  "Used for blocking until all exporters have exited.")
(defvar sdnize-worker-count 0
  "How many workers(Emacs instances) should we use for exporting.")
(defvar sdnize-copy-queue '()
  "Queue of static dependencies to be copied to
the export dir.")
(defvar sdnize-default-exclude
  `("export/"
    "private/"
    "tests/"
    "elpa/"
    "layers/LAYERS.org")
  "List of Spacemacs directories and ORG files that normally
 shouldn't be exported.")

;;; NOTE: Mb move back to the shared file?
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun sdnize/make-file-size-path-alist (files)
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

(defun sdnize/get-cpu-count ()
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

(defun sdnize/find-org-files (paths)
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

(defun sdnize/files-to-buckets (files n)
  "Split FILES into N lists(task buckets) balancing by file sizes."
  (let ((fps-alist (sort
                    (sdnize/make-file-size-path-alist
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
(byte-compile 'sdnize-filse-to-buckets)

(defun sdnize/do-concurrently
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
          (sdnize/files-to-buckets
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

(defun sdnize/copy-file-to-target-dir (file-path)
  "Copy file at FILE-PATH into `sdnize-target-dir'.
ROOT-DIR is the documentation root directory. Empty FILE-PATH ignored."
  (unless (string-empty-p file-path)
    (let* ((op (file-relative-name file-path sdnize-root-dir))
           (np (expand-file-name op sdnize-target-dir))
           (np-dir (file-name-directory np)))
      (make-directory np-dir t)
      (message "Copying file %S into %S" file-path np)
      (copy-file file-path np t))))

(defun sdnize/sentinel (p e)
  "Sentinel for worker process."
  (condition-case err
      (let ((buff (process-buffer p)))
        (if (not (eq (process-status p) 'exit))
            (error "Process %s doesn't have status: exit" p)
          (sdnize/interpret-proc-output p buff)
          (kill-buffer buff))
        (if (string-match-p "finished" e)
            (progn
              (message "Process %s has finished.\n" p)
              (when (= (cl-incf sdnize-workers-fin)
                       sdnize-worker-count)
                (setq sdnize-stop-waiting t)))
          (error "Process %s was %s"
                 p e)
          (setq sdnize-stop-waiting t)))
    (error (setq sdnize-stop-waiting t)
           (error "%s" err))))

(defun sdnize/worker-msg-handler (resp)
  "Process payload received for a worker."
  (let* ((type (alist-get 'type resp))
         ;; Unescape newlines inside payload.
         (text (replace-regexp-in-string
                "{{newline}}"
                "\n"
                (alist-get 'text resp)))
         (msg (cond
               ((string= type "message")
                text)
               ((string= type "warning")
                (concat "\n=============== WARNING ===============\n"
                        text
                        "\n=======================================\n"))
               ((string= type "error")
                (concat "\n!!!!!!!!!!!!!!!! ERROR !!!!!!!!!!!!!!!!\n"
                        text
                        "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"))
               ((string= type "export")
                (if sdnize-copy-assets
                    (format
                     (concat "File %S has static dependency %S\n"
                             "=> it will be copied into the export directory")
                     (alist-get 'source resp)
                     (car (push text sdnize-copy-queue)))
                  ""))
               (t
                (concat "\n?????????? UNKNOWN EVENT TYPE ????????????\n"
                        (format "TYPE:\"%s\" TEXT: \"%s\"" type text)
                        "\n?????????????????????????????????????????\n")))))
    (unless (string= msg "") (message msg))
    (unless (member type '("message" "warning" "export")) (kill-emacs 2))))

(defun sdnize/interpret-proc-output (proc buff)
  "Parses process PROC BUFFER. Process P should be finished."
  (message "PROCESS: %S\n" proc)
  ;; We have one payload per line.
  (dolist (line (split-string (with-current-buffer buff (buffer-string)) "\n"))
    ;; Ignore junk.
    (unless (or (string= line "") (string-match-p "^Loading.*\\.\\.\\.$" line))
      ;; Parse payload
      (let ((resp (ignore-errors (json-read-from-string line))))
        (unless resp (error "Malformed response:%s" line))
        (sdnize/worker-msg-handler resp))))
  (while sdnize-copy-queue
    (sdnize/copy-file-to-target-dir
     (pop sdnize-copy-queue))))

(defun sdnize/build-default-list (root-dir)
  "Create default list of Spacemacs documentation files to export."
  (let ((exclude-re (regexp-opt (mapcar
                                 (apply-partially 'concat root-dir)
                                 sdnize-default-exclude))))
    (delete 0 (mapcar (lambda (path) (or (string-match-p exclude-re path) path))
                      (directory-files-recursively root-dir "\\.org$")))))

(defun sdnize/run (arg-list)
  "Main function for running as a script. ARG-LIST is an argument list.
See `sdnize-help-text' for description."
  (unless arg-list
    (error sdnize-help-text))
  (let ((f-arg (car arg-list)))
    (setq sdnize-copy-assets
          (when (string-prefix-p "+" f-arg)
            ;; NOTE: Weren't planing to have any more options :P
            (if (string= f-arg "+copy-assets")
                (pop arg-list)
              (error "Unrecognized option: %s" f-arg)))))
  (unless (file-directory-p (car arg-list))
    (error "The first argument must be a readable directory."))
  (let ((targt-dir-name (cadr arg-list)))
    (if (and targt-dir-name (directory-name-p targt-dir-name))
        (make-directory targt-dir-name t)
      (error "The second argument must be directory name \"ends with /\"")))
  (setq sdnize-workers-fin 0
        sdnize-stop-waiting nil)
  (let* ((default-directory sdnize-run-file-dir)
         (w-path (progn (byte-compile-file "sdnize_worker.el")
                        (file-truename "sdnize_worker.elc")))
         (root-dir (file-truename (file-name-as-directory (pop arg-list))))
         (target-dir (file-truename (file-name-as-directory (pop arg-list))))
         (files (let ((default-directory root-dir))
                  (sdnize/find-org-files
                   (or arg-list
                       (sdnize/build-default-list root-dir)))))
         (f-length (length files))
         (w-count
          ;; FIXME: With 1-2  workers it gets extremely slow.
          (min (max 4 (sdnize/get-cpu-count)) f-length)))
    (if (= f-length 0)
        (progn (message "No files to export.")
               (kill-emacs 0))
      (setq sdnize-worker-count w-count
            sdnize-root-dir root-dir
            sdnize-target-dir target-dir)
      (sdnize/do-concurrently
       files
       w-count
       w-path
       'sdnize/sentinel
       (lambda (f) (format "%S" `(sdnize/to-sdn ,root-dir ,sdnize-target-dir ',f))))
      (while (not sdnize-stop-waiting)
        (accept-process-output))
      (message "Done."))))

;; Script entry point.
(when (and load-file-name noninteractive)
  (sdnize/run argv))

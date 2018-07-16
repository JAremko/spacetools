;;; shader.el -- shader code of Spacemacs tools -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Commentary:
;;; Code:

(defconst spacemacs--spacetools-lib-dir
  (file-truename
   (file-name-directory
    (or load-file-name
        buffer-file-name)))
  "Path to emacs tools libs directory.")

(defun spacemacs//spacetools-make-file-size-path-alist (files)
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

(defun spacemacs//spacetools-get-cpu-count ()
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

(defun spacemacs//spacetools-find-org-files (paths)
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

(defun spacemacs//spacetools-files-to-buckets (files n)
  "Split FILES into N lists(task buckets) balancing by file sizes."
  (let ((fps-alist (sort
                    (spacemacs//spacetools-make-file-size-path-alist
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
(byte-compile 'spacemacs//spacetools-filse-to-buckets)

(defun spacemacs//spacetools-do-concurrently
    (files w-count w-path make-task sentinel)
  "Run task concurrently.
Process FILES using W-COUNT workers(child emacs processes) loaded from W-PATH
MAKE-TASK is a function that takes single argument (file list) and returns
string representation of a task that each worker will perform - it will be
called as \"emacs -l W-PATH --batch -eval <value returned from MAKE-TASK>\"/.
SENTINEL is a worker process sentinel."
  (let ((file-buckets '())
        (emacs-fp (executable-find "emacs"))
        (toc-org-fp (concat spacemacs--spacetools-lib-dir
                            "toc-org.el")))
    (unless emacs-fp
      (error "Can't find emacs executable"))
    (setq file-buckets
          (spacemacs//spacetools-files-to-buckets
           files
           (min w-count
                (length files))))
    (byte-compile-file toc-org-fp)
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

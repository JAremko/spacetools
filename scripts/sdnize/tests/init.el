(require 'buttercup)

(when (require 'undercover nil t)
  (undercover "sdnize*.el"
              (:report-file "/tmp/cov/coveralls.json")
              (:send-report nil)))

(defconst sdnize-testing t)


(defmacro sdnize-test-worker-fixture (&rest body)
  "Filter worker messages and manage TMP-DIR temporary test directory."
  `(cl-letf ((tmp-dir (concat temporary-file-directory
                              (make-temp-name "sdnize_worker_test")))
             ((symbol-function 'sdnize/message) #'message)
             ((symbol-function 'sdnize/warn) #'message)
             ((symbol-function 'sdnize/error)
              (lambda (format-string &rest args)
                (message (concat (format "current-buffer: %s\n"
                                         (buffer-name))
                                 (format "current-file: %s\n"
                                         (buffer-file-name))
                                 "error: "
                                 (apply 'format format-string args)))
                (kill-emacs 1)))
             ((symbol-function 'sdnize/export-file)
              (lambda (src-file file-path)
                (message "Copying file \"%s\" to \"%s\"" src-file file-path))))
     (unwind-protect
         (progn
           (make-directory tmp-dir)
           (progn ,@body))
       (delete-directory tmp-dir t))))

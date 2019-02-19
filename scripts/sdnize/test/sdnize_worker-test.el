;;; sdnize_worker-test.el --- Tests for sdnize -*- lexical-binding: t; -*-

(require 'sdnize_worker)

(ert-deftest sdnize-worker-to-sdn-test ()
  (cl-letf ((tmp-dir (concat temporary-file-directory
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
                                format-string))
               (kill-emacs 1)))
            ((symbol-function 'sdnize/export-file)
             (lambda (src-file file-path)
               (message "Copying file \"%s\" to \"%s\"" src-file file-path))))
    (unwind-protect
        (progn
          (make-directory tmp-dir)
          (sdnize/to-sdn "test/samples"
                         "/tmp/samples"
                         (directory-files-recursively "test/samples"
                                                      "\\.org$")))
      (delete-directory tmp-dir t))))


(provide 'sdnize_worker-test)

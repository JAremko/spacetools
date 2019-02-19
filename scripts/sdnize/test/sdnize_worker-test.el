;;; sdnize_worker-test.el --- Tests for sdnize -*- lexical-binding: t; -*-

(require 'sdnize_worker)

(ert-deftest sdnize-worker-to-sdn-test ()
  (let ((tmp-dir (concat temporary-file-directory
                         (make-temp-name "sdnize_worker_test"))))
    (unwind-protect
        (progn
          (message "!!!!!!%s!!!!!" tmp-dir)
          (make-directory tmp-dir)
          (sdnize/to-sdn "test/samples"
                         "/tmp/samples"
                         (directory-files-recursively "test/samples"
                                                      "\\.org$")))
      (delete-directory tmp-dir t))))


(provide 'sdnize_worker-test)

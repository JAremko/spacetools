;;; sdnize_worker-test.el --- Tests for sdnize -*- lexical-binding: t; -*-

(require 'sdnize_worker)

(ert-deftest sdnize-worker-to-sdn-test ()
  (thread-last "\\.org$"
    (directory-files-recursively "test/samples")
    (sdnize/to-sdn "test/samples" tmp-dir)
    (sdnize-test-worker-fixture)))


(provide 'sdnize_worker-test)

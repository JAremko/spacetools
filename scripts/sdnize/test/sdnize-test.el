;;; sdnize-test.el --- Tests for sdnize

(require 'sdnize)

(ert-deftest sdnize-test ()
  (should (equal '(("+bar" "+foo") ("baz"))
                 (sdnize/extract-options '("+foo" "+bar" "baz")))))

(provide 'sdnize-test)

;;; sdnize-test.el ends here

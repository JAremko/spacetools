;;; test-helper.el --- Helpers for sdnize-test.el

;;; test-helper.el ends here

(require 'undercover)
(undercover "*.el")

(defconst sdnize-testing t)

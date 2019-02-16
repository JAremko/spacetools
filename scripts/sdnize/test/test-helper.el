;;; test-helper.el --- Helpers for sdnize-test.el

;;; test-helper.el ends here

(require 'undercover)
(undercover "*.el" (:report-file "/tmp/cov/coveralls.json") (:send-report nil))

(defconst sdnize-testing t)

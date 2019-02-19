;;; test-helper.el --- Helpers for sdnize tests -*- lexical-binding: t; -*-

;;; test-helper.el ends here

(require 'undercover)
(undercover "sdnize*.el"
            (:report-file "/tmp/cov/coveralls.json")
            (:send-report nil))

(defconst sdnize-testing t)

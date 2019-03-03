;;; sdnize-runner-test.el --- Tests for sdnize -*- lexical-binding: t; -*-

(load-file "./tests/init.el")

(require 'sdnize)

(describe "Function: `sdnize/extract-options'"
  (it "Should split options from other arguments"
    (expect (sdnize/extract-options '("+foo" "+bar" "baz"))
            :to-have-same-items-as '(("+bar" "+foo") ("baz")))))

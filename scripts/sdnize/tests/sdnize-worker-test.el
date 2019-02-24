;;; sdnize-worker-test.el --- Tests for sdnize -*- lexical-binding: t; -*-

(load-file "./tests/init.el")

(require 'sdnize_worker)

(describe "Function: `sdnize/to-sdn'"
          (it "should work :shiptit:"
              (expect (thread-last "\\.org$"
                        (directory-files-recursively "tests/samples")
                        (sdnize/to-sdn "tests/samples" tmp-dir)
                        (sdnize-test-worker-fixture))
                      :not :to-throw)))

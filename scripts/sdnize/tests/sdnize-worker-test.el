;;; sdnize-worker-test.el --- Tests for sdnize -*- lexical-binding: t; -*-

(load-file "./tests/init.el")

(require 'sdnize_worker)

;; TODO: Add more tests

(describe "Function: `sdnize/to-sdn'"
          :var (tmp-dir sample-fs num-samples)
          (before-all
           (setq tmp-dir (thread-last "sdnize_test"
                           (make-temp-name)
                           (concat temporary-file-directory)
                           (file-name-as-directory))
                 sample-fs (directory-files-recursively sdnize-elems "\\.org$")
                 num-samples (length sample-fs))
           (make-directory tmp-dir)
           (spy-on 'sdnize/message :and-return-value nil)
           (spy-on 'sdnize/warn :and-call-fake #'message)
           (spy-on 'sdnize/error :and-call-fake
                   (lambda (format-string &rest args)
                     (message (concat (format "current-buffer: %s\n"
                                              (buffer-name))
                                      (format "current-file: %s\n"
                                              (buffer-file-name))
                                      "error: "
                                      (apply 'format format-string args)))
                     (kill-emacs 1)))
           (spy-on 'sdnize/export-file :and-return-value nil))
          (after-all (delete-directory tmp-dir t))
          (it "Should succeed with .org samples"
              (expect (sdnize/to-sdn sdnize-elems tmp-dir sample-fs)
                      :not :to-throw))
          (it "target directory should exist"
              (expect (file-directory-p tmp-dir) :to-be-truthy))
          (it "target directory should contain .sdn files"
              (expect (length (directory-files-recursively tmp-dir "\\.sdn$"))
                      :to-be-greater-than 0))
          (it "Every parsed sample"
              (let ((fp->sdn (make-hash-table :test 'equal)))
                (mapc (lambda (fp)
                        (puthash fp (sdnize-test/read-edn-file fp) fp->sdn))
                      (directory-files-recursively tmp-dir "\\.sdn$"))
                (cl-flet ((without-root
                           (thread-last :root
                             (apply-partially #'sdnize-test/sdn-contains-tag)
                             (apply-partially #'sdnize-test/filter-hashtable)))
                          (without-sample-element
                           (h-t)
                           (sdnize-test/filter-hashtable
                            (lambda (v)
                              (thread-first v
                                (sdnize-test/sdn-get-source-dir)
                                (sdnize-test/string->keyword)
                                (sdnize-test/sdn-contains-tag v)))
                            h-t)))
                  (expect (hash-table-count fp->sdn) :to-equal num-samples)
                  (expect (without-root fp->sdn) :to-equal nil)
                  (expect (without-sample-element fp->sdn) :to-equal nil)))))


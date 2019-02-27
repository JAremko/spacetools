;;; init.el --- Tests initialization. -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl)
(require 'edn)

(when (require 'undercover nil t)
  (undercover "sdnize*.el"
              (:report-file "/tmp/cov/coveralls.json")
              (:send-report nil)))

(defconst sdnize-testing t)


;;;; Helpers

(defun sdnize-test/sdn-contains-tag (tag sdn)
  "Return true if SDN contains at least one node with the TAG."
  (when (hash-table-p sdn)
    (or (when (eq (gethash :tag sdn) tag) tag)
        (cl-reduce (lambda (acc child)
                     (or acc
                         (sdnize-test/sdn-contains-tag tag child)))
                   (gethash :children sdn)
                   :initial-value nil))))

(defun sdnize-test/sdn-get-source-dir (sdn)
  "Return source (parent) directory of SDN document."
  (if (and (hash-table-p sdn)
           (eq (gethash :tag sdn) :root))
      (when-let ((source (gethash :source sdn)))
        (thread-first source
          (file-name-directory)
          (directory-file-name)
          (file-name-base)))
    (error "Root element of a SDN file should have :root tag")))

(defun sdnize-test/sdn-get-title (sdn)
  "Return title of SDN document."
  (when (hash-table-p sdn)
    (or (when (and (eq (gethash :tag sdn) :key-word)
                   (string= (gethash :key sdn) "TITLE"))
          (gethash :value sdn))
        (cl-reduce (lambda (acc child)
                     (or acc (sdnize-test/sdn-get-title child)))
                   (gethash :children sdn)
                   :initial-value nil))))

(defun sdnize-test/string->keyword (str)
  "Make keyword from string."
  (intern-soft (concat ":" str)))

(defun sdnize-test/read-edn-file (filePath)
  "Return parsed content of FILEPATH EDN file."
  (edn-read (with-temp-buffer
              (insert-file-contents filePath)
              (buffer-string))))

(defun sdnize-test/filter-hashtable (val-pred h-t)
  "Return list of hash table H-T keys of values satisfying VAL-PRED."
 (let ((ret-col nil))
   (maphash
    (lambda (k v)
      (unless (funcall val-pred v)
        (push k ret-col)))
    h-t)
   ret-col))

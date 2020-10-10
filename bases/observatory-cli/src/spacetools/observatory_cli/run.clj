(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [spacetools.observatory-cli.elisp.core :as el]))

(defn -main [file & args]
  (let [files (->> file
                   clojure.java.io/file
                   file-seq
                   (keep (comp (partial re-matches #".*\.el$") str))
                   (map #(vector % (slurp %))))]
    (time (vec (map #(do (prn (first %))
                         (el/read-str (second %)))
                    files))))
  (prn "Done!"))


(def text
  "Test text"
  ";; (1 2 3) foo
`,@foo #'baz
,@()	
(1.0)	
(defvar configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout	
  \"Timeout in seconds to reach a package archive page.\")	
,bar	
:zzz	
\"fff\"	
     ((file-exists-p layer-dir)	
      (configuration-layer/message	
       (concat \"Cannot create configuration layer \\\"\\\", \"	
               \"this layer already exists.\") name))	
1011;; baz	
   ;; Note:	
(1+	
;; bar	
)	
(+1.0 +2 .0 0.0.0 #24r5 #b0.0 #b111 '() 2+2 2 '2 +1.2b [])	
              (let ((a [1 2 3])) a)	
?b ?  ?? ?\\C-m ?\\\\  ( ?\\\\ )	
'[1 2 3]	
 ((equal event 32) ?')   ; space 	
or()	
\"\\\\\\\\\" a	
\"'\"	
bar'(foo) (quote 10)	
\\\\\\\\\\[foo	
;")

;; (el/read-str text)

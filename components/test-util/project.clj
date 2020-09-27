(defproject spacetools/test-util "0.1"
  :description "A testing utility component."
  :dependencies [[com.google.jimfs/jimfs "1.1"]
                 [environ "1.1.0"]
                 [nio2 "0.2.3"]
                 [orchestra "2020.07.12-1"]
                 [org.clojure/clojure "1.10.2-alpha2"]
                 [org.clojure/test.check "1.1.0"]
                 [spacetools/interfaces "1.0"]]
  :aot :all)

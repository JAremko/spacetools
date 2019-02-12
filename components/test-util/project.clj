(defproject spacetools/test-util "0.1"
  :description "A test-util component."
  :dependencies [[environ "1.1.0"]
                 [nio2 "0.2.1"]
                 [orchestra "2019.02.06-1"]
                 [com.google.jimfs/jimfs "1.1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [spacetools/interfaces "1.0"]]
  :aot :all)

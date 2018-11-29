(defproject spacetools/test-util "0.1"
  :description "A test-util component."
  :dependencies [[environ "1.1.0"]
                 [org.clojure/clojure "1.9.0"]
                 [orchestra "2018.09.10-1"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [com.google.jimfs/jimfs "1.1"]
                 [spacetools/interfaces "1.0"]]
  :aot :all)

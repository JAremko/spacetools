(defproject spacetools/observatory-cli "0.1"
  :description "A observatory-cli base."
  :dependencies [[instaparse "1.4.10"]
                 [org.clojure/clojure "1.10.2-alpha1"]
                 [spacetools/interfaces "1.0"]]
  :main spacetools.observatory-cli.run
  :aot :all)

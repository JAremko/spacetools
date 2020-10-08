(defproject spacetools/observatory-cli "0.1"
  :description "A observatory-cli base."
  :dependencies [[clj-antlr "0.2.6"]
                 [medley "1.3.0"]
                 [orchestra "2020.07.12-1"]
                 [org.clojure/clojure "1.10.2-alpha2"]
                 [spacetools/interfaces "1.0"]]
  :main spacetools.observatory-cli.run
  :aot :all)

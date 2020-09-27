(defproject spacetools/observatory-cli "0.1"
  :description "A observatory-cli system."
  :dependencies [[instaparse "1.4.10"]
                 [orchestra "2020.07.12-1"]
                 [org.clojure/clojure "1.10.2-alpha1"]]
  :aot :all
  :main spacetools.observatory-cli.run)

(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :dependencies [[orchestra "2020.07.12-1"]
                 [funcool/cats "2.3.6"]
                 [org.clojure/clojure "1.10.2-alpha1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/tools.cli "1.0.194"]
                 [spacetools/interfaces "1.0"]]
  :main spacetools.spacedoc-cli.run
  :aot :all)

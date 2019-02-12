(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :dependencies [[orchestra "2019.02.06-1"]
                 [funcool/cats "2.3.2"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/tools.cli "0.4.1"]
                 [spacetools/interfaces "1.0"]]
  :main spacetools.spacedoc-cli.run
  :aot :all)

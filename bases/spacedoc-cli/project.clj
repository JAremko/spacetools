(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :dependencies [[environ "1.1.0"]
                 [orchestra "2018.09.10-1"]
                 [funcool/cats "2.2.0"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/tools.cli "0.3.7"]
                 [spacetools/interfaces "1.0"]]
  :main spacetools.spacedoc-cli.core
  :aot :all)

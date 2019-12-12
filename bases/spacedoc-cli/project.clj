(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :dependencies [[orchestra "development-SNAPSHOT"]
                 [funcool/cats "2.3.3"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/tools.cli "0.4.2"]
                 [spacetools/interfaces "1.0"]]
  :main spacetools.spacedoc-cli.run
  :aot :all)

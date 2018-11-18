(defproject spacetools/development "0.1.0-SNAPSHOT"
  :description "The main development environment."
  :plugins [[lein-environ "1.0.0"]]
  :dependencies [[environ "1.1.0"]
                 [funcool/cats "2.2.0"]
                 [orchestra "2018.09.10-1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [org.clojure/tools.cli "0.3.7"]]
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles {:dev {:jvm-opts ["-Xms2G" "-Xmx2G" "-Xss8m"]}
             :test {:env {:gentest-multiplier "3"}
                    :jvm-opts ["-Xms5G" "-Xmx5G" "-Xss8m"]}}
  :aot :all)

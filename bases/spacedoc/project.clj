(defproject spacetools/spacedoc "0.1.0-SNAPSHOT"
  :dependencies [[environ "1.1.0"]
                 [funcool/cats "2.2.0"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main spacetools.spacedoc.core
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles
  {:dev {:jvm-opts ["-Xms2G" "-Xmx2G" "-Xss8m"]
         :dependencies [[org.clojure/test.check "0.10.0-alpha3"]]}
   :test {:env {:gentest-multiplier "3"}
          :jvm-opts ["-Xms5G" "-Xmx5G" "-Xss8m"]}})

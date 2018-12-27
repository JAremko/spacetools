(defproject spacetools/development "0.1.0-SNAPSHOT"
  :description "The main development environment."
  :plugins [[lein-environ "1.0.0"]]
  :dependencies [[com.google.jimfs/jimfs "1.1"]
                 [environ "1.1.0"]
                 [funcool/cats "2.3.1"]
                 [nio2 "0.2.1"]
                 [orchestra "2018.09.10-1"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [org.clojure/tools.cli "0.4.1"]]
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles {:dev {:jvm-opts ["-Xmn4G" "-Xss8m"]}
             :test {:env {:gentest-multiplier "3"}}}
  :aot :all)

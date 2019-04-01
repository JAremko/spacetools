(defproject spacetools/development "0.1.0-SNAPSHOT"
  :description "The main development environment."
  :dependencies [[cheshire "5.8.1"]
                 [com.google.jimfs/jimfs "1.1"]
                 [environ "1.1.0"]
                 [funcool/cats "2.3.2"]
                 [http-kit "2.4.0-alpha3"]
                 [nio2 "0.2.3"]
                 [orchestra "2019.02.17-SNAPSHOT"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/test.check "0.10.0-alpha4"]
                 [org.clojure/tools.cli "0.4.2"]]
  :uberjar-name "spacedoc.jar"
  :profiles
    {:user {:env {:gentest-multiplier "1"}
            :plugins [[lein-cloverage "1.1.2-SNAPSHOT"]
                      [lein-exec "0.3.7"]
                      [lein-environ "1.1.0"]
                      [lein-nvd "1.0.0"]
                      [lein-deps-tree "0.1.2"]]}
     :dev {:global-vars {*warn-on-reflection* true
                         *assert* true}
           :plugins [[lein-environ "1.1.0"]]
           :dependencies [[com.google.jimfs/jimfs "1.1"]
                          [com.rpl/specter "1.1.3-SNAPSHOT"]
                          [org.clojure/clojure "1.10.0"]
                          [org.clojure/test.check
                           "0.10.0-alpha4"]]
           :jvm-opts ["-Xmn4G" "-Xss8m"]}
     :test {:env {:gentest-multiplier "3"}}}
  :aot :all)

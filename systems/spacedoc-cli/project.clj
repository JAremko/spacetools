(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :description "Documentation tools for Spacemacs."
  :plugins [[lein-environ "1.0.0"]]
  :dependencies [[environ "1.1.0"]
                 [org.clojure/clojure "1.9.0"]
                 [funcool/cats "2.2.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main spacetools.spacedoc-cli.core
  :uberjar-name "spacedoc.jar"
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles
  {:dev {:jvm-opts ["-Xms2G" "-Xmx2G" "-Xss8m"]
         :dependencies [[org.clojure/test.check "0.10.0-alpha3"]]}
   :test {:env {:gentest-multiplier "3"}
          :jvm-opts ["-Xms5G" "-Xmx5G" "-Xss8m"]}
   :uberjar
   {:aot :all
    :jvm-opts
    ["-Dclojure.compiler.elide-meta=[:doc :file :line :added]"
     "-Dclojure.compiler.direct-linking=true" "-Xms1G" "-Xmx1G"]
    :global-vars {*assert* false}}}
  :aot :all)

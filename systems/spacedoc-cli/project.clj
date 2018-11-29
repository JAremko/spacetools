(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :description "CLI tools for Spacemacs documentation."
  :plugins [[lein-environ "1.0.0"]]
  :dependencies [[orchestra "2018.09.10-1"]
                 [funcool/cats "2.3.1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [nio2 "0.2.1"]
                 [org.clojure/tools.cli "0.4.1"]]
  :main spacetools.spacedoc-cli.core
  :uberjar-name "spacedoc.jar"
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles
  {:dev {:jvm-opts ["-Xms2G" "-Xmx2G" "-Xss8m"]
         :dependencies [[org.clojure/test.check "0.10.0-alpha3"]
                        [com.google.jimfs/jimfs "1.1"]]}
   :test {:env {:gentest-multiplier "3"}
          :jvm-opts ["-Xms5G" "-Xmx5G" "-Xss8m"]}
   :uberjar
   {:aot :all
    :jvm-opts
    ["-Dclojure.compiler.elide-meta=[:doc :file :line :added]"
     "-Dclojure.compiler.direct-linking=true" "-Xms1G"
     "-Xmx1G"]
    :global-vars {*assert* false}}}
  :aot :all)

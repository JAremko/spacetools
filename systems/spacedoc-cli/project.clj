(defproject spacetools/spacedoc-cli "0.1.0-SNAPSHOT"
  :description "CLI tools for Spacemacs documentation."
  :plugins [[lein-environ "1.1.0"]]
  :dependencies [[funcool/cats "2.3.6"]
                 [medley "1.3.0"]
                 [nio2 "0.2.3"]
                 [orchestra "2020.07.12-1"]
                 [org.clojure/clojure "1.10.2-alpha2"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/tools.cli "1.0.194"]]
  :main spacetools.spacedoc-cli.run
  :uberjar-name "spacedoc.jar"
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles
    {:dev {:jvm-opts ["-Xss8m"]
           :dependencies [[com.google.jimfs/jimfs "1.1"]
                          [org.clojure/test.check
                           "0.10.0"]]}
     :test {:env {:gentest-multiplier "1"}}
     :uberjar
       {:aot :all
        :jvm-opts
          ["-Dclojure.compiler.elide-meta=[:doc :file :line :added]"
           "-Dclojure.compiler.direct-linking=true"
           "-Xmn1G"]
        :global-vars {*warn-on-reflection* false
                      *assert* false}}})

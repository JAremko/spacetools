(defproject spacetools/observatory-cli "0.1"
  :description "A observatory-cli system."
  :plugins [[lein-environ "1.1.0"]]
  :dependencies [[clj-antlr "0.2.6"]
                 [medley "1.3.0"]
                 [orchestra "2020.07.12-1"]
                 [org.clojure/clojure "1.10.2-alpha2"]]
  :main spacetools.observatory-cli.run
  :uberjar-name "observatory.jar"
  :global-vars {*warn-on-reflection* true *assert* true}
  :profiles
    {:dev {:jvm-opts ["-Xss8m"]
           :dependencies [[org.clojure/test.check
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

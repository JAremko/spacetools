(defproject spacedoc "0.1.0-SNAPSHOT"
  :description "Documentation tools for Spacemacs"
  :url "https://github.com/jaremko/spacedoc"
  :license {:name "GNU General Public License V3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [environ "1.1.0"]
                 [funcool/cats "2.2.0"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main spacedoc.core
  :global-vars {*warn-on-reflection* true
                *assert* true}
  :target-path "target/%s"
  :uberjar-name "sdn.jar"
  :plugins [[lein-environ "1.0.0"]]
  :profiles {:dev {:env {}
                   :jvm-opts ["-Xms2G" "-Xmx2G" "-Xss4m"]
                   :dependencies [[org.clojure/test.check "0.10.0-alpha3"]]}
             :test {:env {:gentest-multiplier "1"}
                    :jvm-opts ["-Xms8G" "-Xmx8G" "-Xss4m"]
                    ;; NOTE: Added this to get more informative spec fails
                    ;;       instead of assert fails on :ret checks
                    ;;       of functions.
                    :global-vars {*assert* false}}
             :uberjar {:aot :all
                       :jvm-opts [(str "-Dclojure.compiler.elide-meta=["
                                       ":doc :file :line :added"
                                       "]")
                                  "-Dclojure.compiler.direct-linking=true"
                                  "-Xms1G"
                                  "-Xmx1G"]
                       :global-vars {*assert* false}}})

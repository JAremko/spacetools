(defproject spacedoc "0.1.0-SNAPSHOT"
  :description "Documentation tools for Spacemacs"
  :url "https://github.com/jaremko/spacedoc"
  :license {:name "GNU General Public License V3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :plugins [[lein-environ "1.0.0"]
            [lein-bikeshed "0.5.1"]
            [lein-git-deps "0.0.1-SNAPSHOT"]]
  :git-dependencies [["https://github.com/funcool/cats.git"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [environ "1.1.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 #_ [funcool/cats "2.3.0"]
                 ;; TODO: Remove stuff below when cats 2.3.0 releases
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/test.check "0.10.0-alpha3"]
                 [manifold "0.1.6"]
                 [funcool/promesa "1.9.0"]]
  :source-paths ["src" ".lein-git-deps/cats/src"]
  :main spacedoc.core
  :global-vars {*warn-on-reflection* true
                *assert* true}
  :target-path "target/%s"
  :uberjar-name "sdn.jar"
  :profiles
  {:dev
   {:env {}
    :jvm-opts ["-Xms2G" "-Xmx2G" "-Xss4m"]
    :dependencies [[org.clojure/test.check "0.10.0-alpha3"]]
    }
   :test
   {:env {:gentest-multiplier "3"}
    :jvm-opts ["-Xms5G" "-Xmx5G" "-Xss4m"]
    ;; NOTE: Added this to get more informative spec fails
    ;;       instead of assert fails on :ret checks
    ;;       of functions.
    :global-vars {*assert* false}}
   :uberjar
   {:aot :all
    :jvm-opts ["-Dclojure.compiler.elide-meta=[:doc :file :line :added]"
               "-Dclojure.compiler.direct-linking=true"
               "-Xms1G"
               "-Xmx1G"]
    :global-vars {*assert* false}}})

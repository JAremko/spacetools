(defproject spacedoc "0.1.0-SNAPSHOT"
  :description "Documentation tools for Spacemacs"
  :url "https://github.com/jaremko/spacedoc"
  :license {:name "GNU General Public License V3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 #_ [lacij/lacij "0.10.0"]
                 [funcool/cats "2.2.0"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main spacedoc.core
  :global-vars {*warn-on-reflection* true}
  :target-path "target/%s"
  :uberjar-name "sdn.jar"
  :profiles {:uberjar {:aot :all}})
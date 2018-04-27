(defproject spacedoc "0.1.0-SNAPSHOT"
  :description "Documentation tools for Spacemacs"
  :url "https://github.com/jaremko/spacedoc"
  :license {:name "GNU General Public License V3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [lacij/lacij "0.10.0"]
                 [failjure "1.3.0"]
                 [org.clojure/tools.cli "0.3.5"]]
  :main spacedoc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

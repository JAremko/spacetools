(defproject spacetools/development "0.1.0-SNAPSHOT"
  :description "The main development environment."
  :dependencies [[cheshire "5.10.0"]
                 [com.google.jimfs/jimfs "1.1"]
                 [com.rpl/specter "1.1.3"]
                 [environ "1.1.0"]
                 [funcool/cats "2.3.6"]
                 [http-kit "2.4.0"]
                 [instaparse "1.4.10"]
                 [medley "1.3.0"]
                 [nio2 "0.2.3"]
                 [orchestra "2020.07.12-1"]
                 [org.clojure/clojure "1.10.2-alpha2"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/tools.cli "1.0.194"]]
  :profiles
    {:user {:env {:gentest-multiplier "1"}
            :test-selectors
              {:no-gen (fn [var-meta & _]
                         (not (clojure.string/ends-with?
                                (:name var-meta)
                                "-gen")))
               :fast (fn [var-meta & _]
                       (not (:slow var-meta)))}
            :plugins [[lein-cloverage "1.2.0"]
                      [lein-exec "0.3.7"]
                      [lein-environ "1.2.0"]
                      [lein-nvd "1.4.1"]
                      [lein-deps-tree "0.1.2"]]}
     :dev {:global-vars {*warn-on-reflection* true
                         *assert* true}
           :plugins [[lein-environ "1.1.0"]]
           :dependencies [[com.google.jimfs/jimfs "1.1"]
                          [http-kit.fake "0.2.2"]
                          [org.clojure/clojure
                           "1.10.2-alpha1"]
                          [org.clojure/core.typed "0.6.0"]
                          [org.clojure/test.check "1.1.0"]]
           :jvm-opts ["-Xmn4G" "-Xss8m"]}
     :test {:env {:gentest-multiplier "3"}}}
  :aot :all)

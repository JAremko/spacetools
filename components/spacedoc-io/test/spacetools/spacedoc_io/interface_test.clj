(ns spacetools.spacedoc-io.interface-test
  (:require [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-io.interface :refer [filesystem]]
            [spacetools.spacedoc-io.interface :as io]
            [spacetools.spacedoc.config :as sc]
            [spacetools.spacedoc.node :as sn]
            [spacetools.test-util.interface :as tu :refer [testing-io]]))


(st/instrument)


(deftest exception-of-macro
  (testing "exception-of macro"
    (is ((io/exception-of? string?) (exc/success "foo")))
    (is ((io/exception-of? string?) (exc/failure (ex-info "foo" {}))))
    (is (not ((io/exception-of? string?) (exc/success 42))))
    (is ((io/exception-of? (s/map-of keyword? int?)) (exc/success {:foo 42})))))


(deftest absolute-fn
  (testing-io "absolute function" []
              [:unix
               (is (= "/work/bar" (str (io/absolute "bar"))))]
              [:osx
               (is (= "/work/bar" (str (io/absolute "bar"))))]
              [:windows
               (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))


(deftest rebase-path-fn
  (testing-io "rebase-path function" []
              [:unix
               (is (= "/bar/baz"
                      (str (io/rebase-path "/foo" "/bar" "/foo/baz"))))
               (is (= "/foo/bar"
                      (str (io/rebase-path "/foo" "/foo" "/foo/bar"))))
               (is (= "/foo/bar"
                      (str (io/rebase-path "/baz" "/qux" "/foo/bar"))))]
              [:osx
               (is (= "/bar/baz"
                      (str (io/rebase-path "/foo" "/bar" "/foo/baz"))))
               (is (= "/foo/bar"
                      (str (io/rebase-path "/foo" "/foo" "/foo/bar"))))
               (is (= "/foo/bar"
                      (str (io/rebase-path "/baz" "/qux" "/foo/bar"))))]
              [:windows
               (is (= "C:\\bar\\baz"
                      (str
                       (io/rebase-path "C:\\foo" "C:\\bar" "C:\\foo\\baz"))))
               (is (= "C:\\foo\\bar"
                      (str
                       (io/rebase-path "C:\\foo" "C:\\foo" "C:\\foo\\bar"))))
               (is (= "C:\\foo\\bar"
                      (str
                       (io/rebase-path "C:\\baz" "C:\\qux" "C:\\foo\\bar"))))]))


(deftest *spit-fn
  (testing-io "*spit function" []
              [:unix
               (is (exc/success? (io/*spit "/foo/bar" "foo\nbar")))
               (is (= (nio/read-all-lines (nio/path filesystem "/foo/bar"))
                      ["foo" "bar"]))]
              [:osx
               (is (exc/success? (io/*spit "/foo/bar" "foo\nbar")))
               (is (= (nio/read-all-lines (nio/path filesystem "/foo/bar"))
                      ["foo" "bar"]))]
              [:windows
               (is (exc/success? (io/*spit "C:\\foo\\bar" "foo\nbar\n")))
               (is (= (nio/read-all-lines (nio/path filesystem "C:\\foo\\bar"))
                      ["foo" "bar"]))]))


(deftest sdn-file?-fn
  (testing-io "sdn-file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix
               (is (io/sdn-file? "/foo/bar.sdn"))
               (is (not (io/sdn-file? 42)))
               (is (not (io/sdn-file? "/qux")))
               (is (not (io/sdn-file? "/foo")))
               (is (not (io/sdn-file? "/foo/baz.edn")))
               (is (not (io/sdn-file? "/foo/qux.sdn")))]
              [:osx
               (is (io/sdn-file? "/foo/bar.sdn"))
               (is (not (io/sdn-file? 42)))
               (is (not (io/sdn-file? "/qux")))
               (is (not (io/sdn-file? "/foo")))
               (is (not (io/sdn-file? "/foo/baz.edn")))
               (is (not (io/sdn-file? "/foo/qux.sdn")))]
              [:windows
               (is (io/sdn-file? "C:\\foo\\bar.sdn"))
               (is (not (io/sdn-file? 42)))
               (is (not (io/sdn-file? "C:\\qux")))
               (is (not (io/sdn-file? "C:\\foo")))
               (is (not (io/sdn-file? "C:\\foo\\baz.edn")))
               (is (not (io/sdn-file? "C:\\foo\\qux.sdn")))]))


(deftest edn-file?-fn
  (testing-io "edn-file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix
               (is (io/edn-file? "/foo/baz.edn"))
               (is (not (io/edn-file? 42)))
               (is (not (io/edn-file? "/qux")))
               (is (not (io/edn-file? "/foo")))
               (is (not (io/edn-file? "/foo/bar.sdn")))
               (is (not (io/edn-file? "/foo/qux.edn")))]
              [:osx
               (is (io/edn-file? "/foo/baz.edn"))
               (is (not (io/edn-file? 42)))
               (is (not (io/edn-file? "/qux")))
               (is (not (io/edn-file? "/foo")))
               (is (not (io/edn-file? "/foo/bar.sdn")))
               (is (not (io/edn-file? "/foo/qux.edn")))]
              [:windows
               (is (io/edn-file? "C:\\foo\\baz.edn"))
               (is (not (io/edn-file? 42)))
               (is (not (io/edn-file? "C:\\qux")))
               (is (not (io/edn-file? "C:\\foo")))
               (is (not (io/edn-file? "C:\\foo\\bar.sdn")))
               (is (not (io/edn-file? "C:\\foo\\qux.edn")))]))


(deftest drectory?-fn
  (testing-io "directory? function" [[:foo [:bar]]]
              [:unix
               (is (io/directory? "/foo"))
               (is (not (io/directory? 42)))
               (is (not (io/directory? "/qux")))
               (is (not (io/directory? "/foo/bar")))]
              [:osx
               (is (io/directory? "/foo"))
               (is (not (io/directory? 42)))
               (is (not (io/directory? "/qux")))
               (is (not (io/directory? "/foo/bar")))]
              [:windows
               (is (io/directory? "C:\\foo"))
               (is (not (io/directory? 42)))
               (is (not (io/directory? "C:\\qux")))
               (is (not (io/directory? "C:\\foo\\bar")))]))


(deftest *flatten-fps
  (testing-io
   "*flatten-fps function" [[:foo.sdn]
                            [:foo.edn]
                            [:bar {:type :dir}]
                            [:baz
                             [:qux.sdn]]]
   [:unix
    (is (exc/success? (io/*flatten-fps ".sdn" [])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/" "/"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/" "/foo.sdn"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (exc/success? (io/*flatten-fps ".edn" ["/foo.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/bar.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/foo/bar"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/" "/bar.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/" "/foo/bar"])))
    (is (= #{"/foo.sdn"} @(io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["/"]) @(io/*flatten-fps ".sdn" ["/" "/"])))
    (is (= #{"/foo.sdn" "/baz/qux.sdn"} @(io/*flatten-fps ".sdn" ["/"])))
    (is (= #{"/foo.edn"} @(io/*flatten-fps ".edn" ["/"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["/bar"])))]
   [:osx
    (is (exc/success? (io/*flatten-fps ".sdn" [])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/" "/"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/" "/foo.sdn"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (exc/success? (io/*flatten-fps ".edn" ["/foo.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/bar.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/foo/bar"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/" "/bar.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["/" "/foo/bar"])))
    (is (= #{"/foo.sdn"} @(io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["/"]) @(io/*flatten-fps ".sdn" ["/" "/"])))
    (is (= #{"/foo.sdn" "/baz/qux.sdn"} @(io/*flatten-fps ".sdn" ["/"])))
    (is (= #{"/foo.edn"} @(io/*flatten-fps ".edn" ["/"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["/bar"])))]
   [:windows
    (is (exc/success? (io/*flatten-fps ".sdn" [])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["C:\\"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["C:\\" "C:\\"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["C:\\" "C:\\foo.sdn"])))
    (is (exc/success? (io/*flatten-fps ".sdn" ["C:\\foo.sdn"])))
    (is (exc/success? (io/*flatten-fps ".edn" ["C:\\foo.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["C:\\bar.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["C:\\foo\\bar"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["C:\\" "C:\\bar.edn"])))
    (is (exc/failure? (io/*flatten-fps ".sdn" ["C:\\" "C:\\foo\\bar"])))
    (is (= #{"C:\\foo.sdn"} @(io/*flatten-fps ".sdn" ["C:\\foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["C:\\"])
           @(io/*flatten-fps ".sdn" ["C:\\" "C:\\"])))
    (is (= #{"C:\\foo.sdn" "C:\\baz\\qux.sdn"}
           @(io/*flatten-fps ".sdn" ["C:\\"])))
    (is (= #{"C:\\foo.edn"} @(io/*flatten-fps ".edn" ["C:\\"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["C:\\bar"])))]))


(deftest *fp->sdn-fn
  (let [valid-sdn (-> (sn/todo "foo")
                      (sn/root)
                      (str))]
    (testing-io "*fp->sdn function" [[:foo.sdn valid-sdn]
                                     [:bar.sdn "{:foo :bar}"]
                                     [:baz.txt valid-sdn]
                                     [:qux.sdn (str valid-sdn valid-sdn)]]
                [:unix
                 (is (exc/success? (io/*fp->sdn "/foo.sdn")))
                 (is (exc/success? (io/*fp->sdn "/baz.txt")))
                 (is (= valid-sdn (str @(io/*fp->sdn "/baz.txt"))))
                 (is (exc/failure? (io/*fp->sdn "/qux.sdn")))
                 (is (exc/failure? (io/*fp->sdn "/")))
                 (is (exc/failure? (io/*fp->sdn "/bar.sdn")))]
                [:osx
                 (is (exc/success? (io/*fp->sdn "/foo.sdn")))
                 (is (exc/success? (io/*fp->sdn "/baz.txt")))
                 (is (= valid-sdn (str @(io/*fp->sdn "/baz.txt"))))
                 (is (exc/failure? (io/*fp->sdn "/qux.sdn")))
                 (is (exc/failure? (io/*fp->sdn "/")))
                 (is (exc/failure? (io/*fp->sdn "/bar.sdn")))]
                [:windows
                 (is (exc/success? (io/*fp->sdn "C:\\foo.sdn")))
                 (is (exc/success? (io/*fp->sdn "C:\\baz.txt")))
                 (is (= valid-sdn (str @(io/*fp->sdn "C:\\baz.txt"))))
                 (is (exc/failure? (io/*fp->sdn "C:\\qux.sdn")))
                 (is (exc/failure? (io/*fp->sdn "C:\\")))
                 (is (exc/failure? (io/*fp->sdn "C:\\bar.sdn")))])))


(deftest *read-cfg-overrides-fn
  (testing-io "*read-cfg-overrides function" [[:foo.edn (str sc/default-config)]
                                              [:bar.edn "{:foo :bar}"]]
              [:unix
               (is (exc/success? (io/*read-cfg-overrides "/foo.edn")))
               (is (s/valid? :spacetools.spacedoc.config/overriding-configs
                             @(io/*read-cfg-overrides "/foo.edn")))
               (is (exc/exception? (io/*read-cfg-overrides "/")))
               (is (exc/exception? (io/*read-cfg-overrides "/bar.edn")))]
              [:osx
               (is (exc/success? (io/*read-cfg-overrides "/foo.edn")))
               (is (s/valid? :spacetools.spacedoc.config/overriding-configs
                             @(io/*read-cfg-overrides "/foo.edn")))
               (is (exc/exception? (io/*read-cfg-overrides "/")))
               (is (exc/exception? (io/*read-cfg-overrides "/bar.edn")))]
              [:windows
               (is (exc/success? (io/*read-cfg-overrides "C:\\foo.edn")))
               (is (s/valid? :spacetools.spacedoc.config/overriding-configs
                             @(io/*read-cfg-overrides "C:\\foo.edn")))
               (is (exc/exception? (io/*read-cfg-overrides "C:\\")))
               (is (exc/exception? (io/*read-cfg-overrides "C:\\bar.edn")))]))


(deftest try-m->output-fn
  (testing "Testing try-m->output function"
    (let [ok-out (new java.io.StringWriter)
          err-out (new java.io.StringWriter)]
      (binding [*out* ok-out
                *err* err-out]
        (with-redefs-fn {#'spacetools.spacedoc-io.core/exit (constantly nil)}
          #(do (io/try-m->output (exc/success "foo"))
               (io/try-m->output (exc/failure (ex-info "bar" {})))
               (is (str/includes? (str ok-out) "foo"))
               (is (not (str/includes? (str ok-out) "bar")))
               (is (str/includes? (str err-out) "bar"))
               (is (not (str/includes? (str err-out) "foo")))))))))

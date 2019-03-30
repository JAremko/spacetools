(ns spacetools.fs-io.interface-test
  "Testing interface of the `fs-io` component."
  (:require [cats.monad.exception :as exc :refer [success? failure?]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.interface :as io :refer [filesystem]]
            [spacetools.test-util.interface :refer [testing-io]]))


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
               (is (success? (io/*spit "/foo/bar" "foo\nbar")))
               (is (= (nio/read-all-lines (nio/path filesystem "/foo/bar"))
                      ["foo" "bar"]))]
              [:osx
               (is (success? (io/*spit "/foo/bar" "foo\nbar")))
               (is (= (nio/read-all-lines (nio/path filesystem "/foo/bar"))
                      ["foo" "bar"]))]
              [:windows
               (is (success? (io/*spit "C:\\foo\\bar" "foo\nbar\n")))
               (is (= (nio/read-all-lines (nio/path filesystem "C:\\foo\\bar"))
                      ["foo" "bar"]))]))


(deftest *slurp-fn
  (testing-io "*slurp function" [[:foo "bar\nbaz"]]
              [:unix
               (is (success? (io/*slurp "/foo")))
               (is (= ["bar" "baz"] @(io/*slurp "/foo")))
               (is (exc/failure (io/*slurp "/")))
               (is (exc/failure (io/*slurp "/non-existing")))]
              [:osx
               (is (success? (io/*slurp "/foo")))
               (is (= ["bar" "baz"] @(io/*slurp "/foo")))
               (is (exc/failure (io/*slurp "/")))
               (is (exc/failure (io/*slurp "/non-existing")))]
              [:windows
               (is (success? (io/*slurp "C:\\foo")))
               (is (= ["bar" "baz"] @(io/*slurp "C:\\foo")))
               (is (exc/failure (io/*slurp "C:\\")))
               (is (exc/failure (io/*slurp "C:\\non-existing")))]))


(deftest *slurp-*spit
  (testing-io "Slurping the spit." []
              [:unix
               (is (= (exc/success ["disgusting"])
                      (io/*slurp @(io/*spit "/dev/mouth" "disgusting"))))]
              [:osx
               (is (= (exc/success ["disgusting"])
                      (io/*slurp @(io/*spit "/dev/mouth" "disgusting"))))]
              [:windows
               (is (= (exc/success ["disgusting"])
                      (io/*slurp @(io/*spit "C:\\dev\\mouth" "disgusting"))))]))


(deftest file?-fn
  (testing-io "file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix
               (is (io/file? "/foo/bar.sdn"))
               (is (io/file? "/foo/baz.edn"))
               (is (not (io/file? 42)))
               (is (not (io/file? "/qux")))
               (is (not (io/file? "/foo")))
               (is (not (io/file? "/foo/qux.sdn")))]
              [:osx
               (is (io/file? "/foo/bar.sdn"))
               (is (io/file? "/foo/baz.edn"))
               (is (not (io/file? 42)))
               (is (not (io/file? "/qux")))
               (is (not (io/file? "/foo")))
               (is (not (io/file? "/foo/qux.sdn")))]
              [:windows
               (is (io/file? "C:\\foo\\bar.sdn"))
               (is (io/file? "C:\\foo\\baz.edn"))
               (is (not (io/file? 42)))
               (is (not (io/file? "C:\\qux")))
               (is (not (io/file? "C:\\foo")))
               (is (not (io/file? "C:\\foo\\qux.sdn")))]))


(deftest file-ref?-fn
  (testing-io "file-ref? function" [[:foo [:bar.txt]]]
              [:unix
               (is (not (io/file-ref? 5)))
               (is (io/file-ref? "foo") "strings are file references")
               (is (io/file-ref? (nio/path filesystem "/foo")))
               (is (io/file-ref? (nio/path filesystem "/non-existent")))]
              [:osx
               (is (not (io/file-ref? 5)))
               (is (io/file-ref? "foo") "strings are file references")
               (is (io/file-ref? (nio/path filesystem "/foo")))
               (is (io/file-ref? (nio/path filesystem "/non-existent")))]
              [:windows
               (is (not (io/file-ref? 5)))
               (is (io/file-ref? "foo") "strings are file references")
               (is (io/file-ref? (nio/path filesystem "C:\\foo")))
               (is (io/file-ref? (nio/path filesystem "C:\\non-existent")))]))


(deftest file-ref->path-fn
  (testing-io "file-ref->path function" [[:foo {:type :dir}]]
              [:unix
               (is (io/file-ref? "foo"))
               (is (io/file-ref? (nio/path filesystem "/foo")))
               (is (io/file-ref? "/non-existent"))
               (is (io/file-ref? (nio/path filesystem "/non-existent")))
               (is (thrown? Exception (io/file-ref->path 42)))]
              [:osx
               (is (io/file-ref? "foo"))
               (is (io/file-ref? (nio/path filesystem "/foo")))
               (is (io/file-ref? "/non-existent"))
               (is (io/file-ref? (nio/path filesystem "/non-existent")))
               (is (thrown? Exception (io/file-ref->path 42)))]
              [:windows
               (is (io/file-ref? "foo"))
               (is (io/file-ref? (nio/path filesystem "C:\\foo")))
               (is (io/file-ref? "C:\\non-existent"))
               (is (io/file-ref? (nio/path filesystem "C:\\non-existent")))
               (is (thrown? Exception (io/file-ref->path 42)))]))


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
    (is (success? (io/*flatten-fps ".sdn" [])))
    (is (success? (io/*flatten-fps ".sdn" ["/"])))
    (is (success? (io/*flatten-fps ".sdn" ["/" "/"])))
    (is (success? (io/*flatten-fps ".sdn" ["/" "/foo.sdn"])))
    (is (success? (io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (success? (io/*flatten-fps ".edn" ["/foo.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/bar.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/foo/bar"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/" "/bar.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/" "/foo/bar"])))
    (is (= #{"/foo.sdn"} @(io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["/"]) @(io/*flatten-fps ".sdn" ["/" "/"])))
    (is (= #{"/foo.sdn" "/baz/qux.sdn"} @(io/*flatten-fps ".sdn" ["/"])))
    (is (= #{"/foo.edn"} @(io/*flatten-fps ".edn" ["/"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["/bar"])))]
   [:osx
    (is (success? (io/*flatten-fps ".sdn" [])))
    (is (success? (io/*flatten-fps ".sdn" ["/"])))
    (is (success? (io/*flatten-fps ".sdn" ["/" "/"])))
    (is (success? (io/*flatten-fps ".sdn" ["/" "/foo.sdn"])))
    (is (success? (io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (success? (io/*flatten-fps ".edn" ["/foo.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/bar.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/foo/bar"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/" "/bar.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["/" "/foo/bar"])))
    (is (= #{"/foo.sdn"} @(io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["/"]) @(io/*flatten-fps ".sdn" ["/" "/"])))
    (is (= #{"/foo.sdn" "/baz/qux.sdn"} @(io/*flatten-fps ".sdn" ["/"])))
    (is (= #{"/foo.edn"} @(io/*flatten-fps ".edn" ["/"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["/bar"])))]
   [:windows
    (is (success? (io/*flatten-fps ".sdn" [])))
    (is (success? (io/*flatten-fps ".sdn" ["C:\\"])))
    (is (success? (io/*flatten-fps ".sdn" ["C:\\" "C:\\"])))
    (is (success? (io/*flatten-fps ".sdn" ["C:\\" "C:\\foo.sdn"])))
    (is (success? (io/*flatten-fps ".sdn" ["C:\\foo.sdn"])))
    (is (success? (io/*flatten-fps ".edn" ["C:\\foo.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["C:\\bar.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["C:\\foo\\bar"])))
    (is (failure? (io/*flatten-fps ".sdn" ["C:\\" "C:\\bar.edn"])))
    (is (failure? (io/*flatten-fps ".sdn" ["C:\\" "C:\\foo\\bar"])))
    (is (= #{"C:\\foo.sdn"} @(io/*flatten-fps ".sdn" ["C:\\foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["C:\\"])
           @(io/*flatten-fps ".sdn" ["C:\\" "C:\\"])))
    (is (= #{"C:\\foo.sdn" "C:\\baz\\qux.sdn"}
           @(io/*flatten-fps ".sdn" ["C:\\"])))
    (is (= #{"C:\\foo.edn"} @(io/*flatten-fps ".edn" ["C:\\"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["C:\\bar"])))]))


(deftest try-m->output-fn
  (testing "Testing try-m->output function"
    (let [ok-out (new java.io.StringWriter)
          err-out (new java.io.StringWriter)]
      (binding [*out* ok-out
                *err* err-out]
        (with-redefs-fn {#'spacetools.fs-io.core/exit (constantly nil)}
          #(do (io/try-m->output (exc/success "foo"))
               (io/try-m->output (exc/failure (ex-info "bar" {})))
               (is (str/includes? (str ok-out) "foo"))
               (is (not (str/includes? (str ok-out) "bar")))
               (is (str/includes? (str err-out) "bar"))
               (is (not (str/includes? (str err-out) "foo")))))))))

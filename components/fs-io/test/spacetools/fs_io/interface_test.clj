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
    (are [pred x] (pred ((io/exception-of? string?) x))
      true? (exc/success "foo")
      true? (exc/failure (ex-info "foo" {}))
      true? (exc/failure (Exception. "foo"))
      false? (exc/success 42))
    (is ((io/exception-of? (s/map-of keyword? int?)) (exc/success {:foo 42})))))


(deftest absolute-fn
  (testing-io "absolute function" []
              ;; NOTE: "work" is a default working directory for jimfs
              [:unix+osx
               (is (= "/work/bar" (str (io/absolute "/work/bar"))))
               (is (= "/work/bar" (str (io/absolute "bar"))))]
              [:windows
               (is (= "C:\\work\\bar" (str (io/absolute "C:\\work\\bar"))))
               (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))


(deftest rebase-path-fn
  (testing-io "rebase-path function" []
              [:unix+osx
               (are [new-path old-base new-base path]
                   (= new-path (str (io/rebase-path old-base new-base path)))
                 "/bar/baz" "/foo" "/bar" "/foo/baz"
                 "/foo/bar" "/foo" "/foo" "/foo/bar"
                 "/foo/bar" "/baz" "/qux" "/foo/bar")]
              [:windows
               (are [new-path old-base new-base path]
                   (= new-path (str (io/rebase-path old-base new-base path)))
                 "C:\\bar\\baz" "C:\\foo" "C:\\bar" "C:\\foo\\baz"
                 "C:\\foo\\bar" "C:\\foo" "C:\\foo" "C:\\foo\\bar"
                 "C:\\foo\\bar" "C:\\baz" "C:\\qux" "C:\\foo\\bar")]))


(deftest *spit-fn
  (testing-io "*spit function" []
              [:unix+osx
               (is (success? (io/*spit "/foo/bar" "foo\nbar")))
               (is (= (nio/read-all-lines (nio/path filesystem "/foo/bar"))
                      ["foo" "bar"]))]
              [:windows
               (is (success? (io/*spit "C:\\foo\\bar" "foo\nbar\n")))
               (is (= (nio/read-all-lines (nio/path filesystem "C:\\foo\\bar"))
                      ["foo" "bar"]))]))


(deftest *slurp-fn
  (testing-io "*slurp function" [[:foo "bar\nbaz"]]
              [:unix+osx
               (is (= ["bar" "baz"] @(io/*slurp "/foo")))
               (are [pred file-path] (true? (pred (io/*slurp file-path)))
                 success? "/foo"
                 failure? "/"
                 failure? "/non-existing")]
              [:windows
               (is (= ["bar" "baz"] @(io/*slurp "C:\\foo")))
               (are [pred file-path] (true? (pred (io/*slurp file-path)))
                 success? "C:\\foo"
                 failure? "C:\\"
                 failure? "C:\\non-existing")]))


(deftest *slurp-*spit
  (testing-io "Slurping the spit." []
              [:unix+osx
               (is (= (exc/success ["disgusting"])
                      (io/*slurp @(io/*spit "/dev/mouth" "disgusting"))))]
              [:windows
               (is (= (exc/success ["disgusting"])
                      (io/*slurp @(io/*spit "C:\\dev\\mouth" "disgusting"))))]))


(deftest file?-fn
  (testing-io "file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix+osx
               (are [pred file-path] (pred (io/file? file-path))
                 true? "/foo/bar.sdn"
                 true? "/foo/baz.edn"
                 false? 42
                 false? "/qux"
                 false? "/foo"
                 false? "/foo/qux.sdn")]
              [:windows
               (are [pred file-path] (pred (io/file? file-path))
                 true? "C:\\foo\\bar.sdn"
                 true? "C:\\foo\\baz.edn"
                 false? 42
                 false? "C:\\qux"
                 false? "C:\\foo"
                 false? "C:\\foo\\qux.sdn")]))


(deftest file-ref?-fn
  (testing-io "file-ref? function" [[:foo [:bar.txt]]]
              [:unix+osx
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
              [:unix+osx
               (are [file-path] (true? (io/file-ref? file-path))
                 "foo"
                 (nio/path filesystem "/foo")
                 "/non-existent"
                 (nio/path filesystem "/non-existent"))
               (is (thrown? Exception (io/file-ref->path 42)))]
              [:windows
               (are [file-path] (true? (io/file-ref? file-path))
                 "foo"
                 (nio/path filesystem "C:\\foo")
                 "C:\\non-existent"
                 (nio/path filesystem "C:\\non-existent"))
               (is (thrown? Exception (io/file-ref->path 42)))]))


(deftest sdn-file?-fn
  (testing-io "sdn-file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix+osx
               (are [pred file-path] (pred (io/sdn-file? file-path))
                 true? "/foo/bar.sdn"
                 false? 42
                 false? "/qux"
                 false? "/foo"
                 false? "/foo/baz.edn"
                 false? "/foo/qux.sdn")]
              [:windows
               (are [pred file-path] (pred (io/sdn-file? file-path))
                 true? "C:\\foo\\bar.sdn"
                 false?  42
                 false?  "C:\\qux"
                 false?  "C:\\foo"
                 false?  "C:\\foo\\baz.edn"
                 false?  "C:\\foo\\qux.sdn")]))


(deftest edn-file?-fn
  (testing-io "edn-file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix+osx
               (are [pred file-path] (pred (io/edn-file? file-path))
                 true? "/foo/baz.edn"
                 false? 42
                 false? "/qux"
                 false? "/foo"
                 false? "/foo/bar.sdn"
                 false? "/foo/qux.edn")]
              [:windows
               (are [pred file-path] (pred (io/edn-file? file-path))
                 true? "C:\\foo\\baz.edn"
                 false? 42
                 false? "C:\\qux"
                 false? "C:\\foo"
                 false? "C:\\foo\\bar.sdn"
                 false? "C:\\foo\\qux.edn")]))


(deftest drectory?-fn
  (testing-io "directory? function" [[:foo [:bar]]]
              [:unix+osx
               (are [pred file-path] (pred (io/directory? file-path))
                 true? "/foo"
                 false? 42
                 false? "/qux"
                 false? "/foo/bar")]
              [:windows
               (are [pred file-path] (pred (io/directory? file-path))
                 true? "C:\\foo"
                 false? 42
                 false? "C:\\qux"
                 false? "C:\\foo\\bar")]))


(deftest *flatten-fps
  (testing-io
   "*flatten-fps function" [[:foo.sdn]
                            [:foo.edn]
                            [:bar {:type :dir}]
                            [:baz
                             [:qux.sdn]]]
   [:unix+osx
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

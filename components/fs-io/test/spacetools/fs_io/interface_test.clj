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
               (are [pred x] (pred (io/file? x))
                 true? "/foo/bar.sdn"
                 true? "/foo/baz.edn"
                 false? 42
                 false? "/qux"
                 false? "/foo"
                 false? "/foo/qux.sdn")]
              [:windows
               (are [pred x] (pred (io/file? x))
                 true? "C:\\foo\\bar.sdn"
                 true? "C:\\foo\\baz.edn"
                 false? 42
                 false? "C:\\qux"
                 false? "C:\\foo"
                 false? "C:\\foo\\qux.sdn")]))


(deftest file-ref?-fn
  (testing-io "file-ref? function" [[:foo [:bar.txt]]]
              [:unix+osx
               (are [pred x] (pred (io/file-ref? x))
                 false? 5
                 true? "foo"
                 true? (nio/path filesystem "/foo")
                 true? (nio/path filesystem "/non-existent"))]
              [:windows
               (are [pred x] (pred (io/file-ref? x))
                 false? 5
                 true? "foo"
                 true? (nio/path filesystem "C:\\foo")
                 true? (nio/path filesystem "C:\\non-existent"))]))


(deftest file-ref->path-fn
  (testing-io "file-ref->path function" [[:foo {:type :dir}]]
              [:unix+osx
               (are [pred file-path] (pred (io/file-ref? file-path))
                 true? "foo"
                 true? (nio/path filesystem "/foo")
                 true? "/non-existent"
                 true? (nio/path filesystem "/non-existent"))
               (is (thrown? Exception (io/file-ref->path 42)))]
              [:windows
               (are [pred file-path] (pred (io/file-ref? file-path))
                 true? "foo"
                 true? (nio/path filesystem "C:\\foo")
                 true? "C:\\non-existent"
                 true? (nio/path filesystem "C:\\non-existent"))
               (is (thrown? Exception (io/file-ref->path 42)))]))


(deftest sdn-file?-fn
  (testing-io "sdn-file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix+osx
               (are [pred x] (pred (io/sdn-file? x))
                 true? "/foo/bar.sdn"
                 false? 42
                 false? "/qux"
                 false? "/foo"
                 false? "/foo/baz.edn"
                 false? "/foo/qux.sdn")]
              [:windows
               (are [pred x] (pred (io/sdn-file? x))
                 true? "C:\\foo\\bar.sdn"
                 false?  42
                 false?  "C:\\qux"
                 false?  "C:\\foo"
                 false?  "C:\\foo\\baz.edn"
                 false?  "C:\\foo\\qux.sdn")]))


(deftest edn-file?-fn
  (testing-io "edn-file? function" [[:foo [:bar.sdn] [:baz.edn]]]
              [:unix+osx
               (are [pred x] (pred (io/edn-file? x))
                 true? "/foo/baz.edn"
                 false? 42
                 false? "/qux"
                 false? "/foo"
                 false? "/foo/bar.sdn"
                 false? "/foo/qux.edn")]
              [:windows
               (are [pred x] (pred (io/edn-file? x))
                 true? "C:\\foo\\baz.edn"
                 false? 42
                 false? "C:\\qux"
                 false? "C:\\foo"
                 false? "C:\\foo\\bar.sdn"
                 false? "C:\\foo\\qux.edn")]))


(deftest drectory?-fn
  (testing-io "directory? function" [[:foo [:bar]]]
              [:unix+osx
               (are [pred x] (pred (io/directory? x))
                 true? "/foo"
                 false? 42
                 false? "/qux"
                 false? "/foo/bar")]
              [:windows
               (are [pred x] (pred (io/directory? x))
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
    (are [pred ext file-paths] (true? (pred (io/*flatten-fps ext file-paths)))
      success? ".sdn" []
      success? ".sdn" ["/"]
      success? ".sdn" ["/" "/"]
      success? ".sdn" ["/" "/foo.sdn"]
      success? ".sdn" ["/foo.sdn"]
      success? ".edn" ["/foo.edn"]
      failure? ".sdn" ["/bar.edn"]
      failure? ".sdn" ["/foo/bar"]
      failure? ".sdn" ["/" "/bar.edn"]
      failure? ".sdn" ["/" "/foo/bar"])
    (is (= #{"/foo.sdn"} @(io/*flatten-fps ".sdn" ["/foo.sdn"])))
    (is (= @(io/*flatten-fps ".sdn" ["/"]) @(io/*flatten-fps ".sdn" ["/" "/"])))
    (is (= #{"/foo.sdn" "/baz/qux.sdn"} @(io/*flatten-fps ".sdn" ["/"])))
    (is (= #{"/foo.edn"} @(io/*flatten-fps ".edn" ["/"])))
    (is (= #{} @(io/*flatten-fps ".sdn" ["/bar"])))]
   [:windows
    (are [pred ext file-paths] (true? (pred (io/*flatten-fps ext file-paths)))
      success? ".sdn" []
      success? ".sdn" ["C:\\"]
      success? ".sdn" ["C:\\" "C:\\"]
      success? ".sdn" ["C:\\" "C:\\foo.sdn"]
      success? ".sdn" ["C:\\foo.sdn"]
      success? ".edn" ["C:\\foo.edn"]
      failure? ".sdn" ["C:\\bar.edn"]
      failure? ".sdn" ["C:\\foo\\bar"]
      failure? ".sdn" ["C:\\" "C:\\bar.edn"]
      failure? ".sdn" ["C:\\" "C:\\foo\\bar"])
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
        (with-redefs-fn {#'spacetools.fs-io.core/exit identity}
          #(do (is (zero? (io/try-m->output (exc/success "foo"))))
               (is (pos? (io/try-m->output (exc/failure (ex-info "bar" {})))))
               (are [pred out sub-out] (pred (str/includes? (str out) sub-out))
                 true? ok-out "foo"
                 false? ok-out "bar"
                 true? err-out "bar"
                 false? err-out "foo")))))))

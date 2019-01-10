(ns spacetools.spacedoc-io.interface-test
  (:require [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-io.interface :refer [filesystem]]
            [spacetools.spacedoc-io.interface :as io]
            [spacetools.spacedoc.node :as sn]
            [spacetools.test-util.interface :as tu :refer [testing-io]]))


(st/instrument)


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
               (is (and (exc/success? (io/*spit "/foo/bar" "foo\nbar"))))
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


(deftest *sdn-fps-in-dir-fn
  (testing-io "*sdn-fps-in-dir function" [[:foo.sdn]
                                          [:foo.edn]
                                          [:bar {:type :dir}]
                                          [:baz
                                           [:qux.sdn]]]
              [:unix
               (is (exc/success? (io/*sdn-fps-in-dir "/")))
               (is (exc/failure? (io/*sdn-fps-in-dir "/foo.sdn")))
               (is (exc/failure? (io/*sdn-fps-in-dir "/qux")))
               (is (= #{} @(io/*sdn-fps-in-dir "/bar")))
               (is (= #{"/baz/qux.sdn"} @(io/*sdn-fps-in-dir "/baz")))
               (is (= #{"/foo.sdn" "/baz/qux.sdn"}
                      @(io/*sdn-fps-in-dir "/")))]
              [:osx
               (is (exc/success? (io/*sdn-fps-in-dir "/")))
               (is (exc/failure? (io/*sdn-fps-in-dir "/foo.sdn")))
               (is (exc/failure? (io/*sdn-fps-in-dir "/qux")))
               (is (= #{} @(io/*sdn-fps-in-dir "/bar")))
               (is (= #{"/baz/qux.sdn"} @(io/*sdn-fps-in-dir "/baz")))
               (is (= #{"/foo.sdn" "/baz/qux.sdn"}
                      @(io/*sdn-fps-in-dir "/")))]
              [:windows
               (is (exc/success? (io/*sdn-fps-in-dir "C:\\")))
               (is (exc/failure? (io/*sdn-fps-in-dir "C:\\foo.sdn")))
               (is (exc/failure? (io/*sdn-fps-in-dir "C:\\qux")))
               (is (= #{} @(io/*sdn-fps-in-dir "C:\\bar")))
               (is (= #{"C:\\baz\\qux.sdn"} @(io/*sdn-fps-in-dir "C:\\baz")))
               (is (= #{"C:\\foo.sdn" "C:\\baz\\qux.sdn"}
                      @(io/*sdn-fps-in-dir "C:\\")))]))


(deftest *fp->sdn
  (let [valid-sdn (-> (sn/todo "foo")
                      (sn/root)
                      (str))]
    (testing-io "*fp->sdn function" [[:foo.sdn valid-sdn]
                                     [:bar.sdn "{:foo :bar}"]
                                     [:baz.qux valid-sdn]]
                [:unix
                 (is (exc/success? (io/*fp->sdn "/foo.sdn")))
                 (is (exc/success? (io/*fp->sdn "/baz.qux")))
                 (is (= valid-sdn (str @(io/*fp->sdn "/baz.qux"))))
                 (is (exc/failure? (io/*fp->sdn "/")))
                 (is (exc/failure? (io/*fp->sdn "/bar.sdn")))]
                [:osx
                 (is (exc/success? (io/*fp->sdn "/foo.sdn")))
                 (is (exc/success? (io/*fp->sdn "/baz.qux")))
                 (is (= valid-sdn (str @(io/*fp->sdn "/baz.qux"))))
                 (is (exc/failure? (io/*fp->sdn "/")))
                 (is (exc/failure? (io/*fp->sdn "/bar.sdn")))]
                [:windows
                 (is (exc/success? (io/*fp->sdn "C:\\foo.sdn")))
                 (is (exc/success? (io/*fp->sdn "C:\\baz.qux")))
                 (is (= valid-sdn (str @(io/*fp->sdn "C:\\baz.qux"))))
                 (is (exc/failure? (io/*fp->sdn "C:\\")))
                 (is (exc/failure? (io/*fp->sdn "C:\\bar.sdn")))])))


;; (testing-io "try-m->output" [] :unix
;;             )
;; (testing-io "*read-cfg-overrides" [] :unix
;;             )

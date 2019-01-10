(ns spacetools.spacedoc-io.interface-test
  (:require [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-io.core :refer [filesystem]]
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

(deftest sdn-file?
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

;; (testing-io "directory?" [] :unix
;;             )
;; (testing-io "*sdn-fps-in-dir" [] :unix
;;             )
;; (testing-io "*fp->sdn" [] :unix
;;             )
;; (testing-io "try-m->output" [] :unix
;;             )
;; (testing-io "*read-cfg-overrides" [] :unix
;;             )

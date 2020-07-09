(ns spacetools.test-util.core-test
  "Tests of testing utility component."
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.core :refer [filesystem]]
            [spacetools.test-util.core :as tu]))


(st/instrument)


;; NOTE: This component consists primary out of thin wrappers over well tested
;; third party code so the tests below are focused on making sure that the
;; wiring done right.


(deftest samples-fn
  (testing "samples function"
    (is (pos-int? (tu/samples 1)))
    (is (zero? (rem (tu/samples 3) 3)))))


(deftest make-f-spec-reper-fn
  (testing "make-f-spec-reper function"
    (is (function? (tu/make-f-spec-reper
                    (s/spec any?)
                    #'spacetools.test-util.core/make-f-spec-reper
                    "foo")))))


(deftest create-fs-fn
  (testing "create-fs function"
    (is (every? tu/filesystem? (map (partial tu/create-fs [])
                                    [:unix :osx :windows])))))


(deftest filesystem-io
  (testing "Im memory file system (jimfs) I/O [UNIX]"
    (let [fs (tu/create-fs [[:path
                             [:to
                              [:text-file-a "line 1" "line 2"]
                              [:text-file-b "line 1" "line 2"]]]]
                           :unix)]
      (is ((complement nio/dir?) (nio/path fs "/path/to/nowhere")))
      (is (nio/dir? (nio/path fs "/path/to")))
      (let [a-file (nio/path fs "/path/to/text-file-a")]
        (is (nio/file? a-file))
        (is ((complement nio/file?)
             (do (nio/delete a-file)
                 a-file))))
      (is (= (nio/read-all-lines (nio/path fs "/path/to/text-file-b"))
             ["line 1" "line 2"]))
      (is (nio/dir? (nio/path fs "/path/to")))))
  (testing "Im memory file system (jimfs) I/O [OSX]"
    (let [fs (tu/create-fs [[:path
                             [:to
                              [:text-file-a "line 1" "line 2"]
                              [:text-file-b "line 1" "line 2"]]]]
                           :osx)]
      (is ((complement nio/dir?) (nio/path fs "/path/to/nowhere")))
      (is (nio/dir? (nio/path fs "/path/to")))
      (let [a-file (nio/path fs "/path/to/text-file-a")]
        (is (nio/file? a-file))
        (is ((complement nio/file?)
             (do (nio/delete a-file)
                 a-file))))
      (is (= (nio/read-all-lines (nio/path fs "/path/to/text-file-b"))
             ["line 1" "line 2"]))
      (is (nio/dir? (nio/path fs "/path/to")))))
  (testing "Im memory file system (jimfs) I/O [WINDOWS]"
    (let [fs (tu/create-fs [[:path
                             [:to
                              [:text-file-a "line 1" "line 2"]
                              [:text-file-b "line 1" "line 2"]]]]
                           :windows)]
      (is ((complement nio/dir?) (nio/path fs "C:\\path\\to\\nowhere")))
      (is (nio/dir? (nio/path fs "C:\\path\\to\\")))
      (let [a-file (nio/path fs "C:\\path\\to\\text-file-a")]
        (is (nio/file? a-file))
        (is ((complement nio/file?)
             (do (nio/delete a-file)
                 a-file))))
      (is (= (nio/read-all-lines (nio/path fs "C:\\path\\to\\text-file-b"))
             ["line 1" "line 2"]))
      (is (nio/dir? (nio/path fs "C:\\path\\to"))))))


(deftest testing-io-macro
  (testing "testing-io macro"
    (letfn [(f-line [path] (->> path
                                (nio/path filesystem)
                                (nio/read-all-lines)
                                (first)))]
      (is (thrown? Exception
                   (tu/testing-io "foo" [[:foo [:bar "baz"]]]
                                  [:bad (is (= "baz" (f-line "/foo/bar")))])))
      (is (tu/testing-io "foo" [[:foo [:bar "baz"]]]
                         [:unix (is (= "baz" (f-line "/foo/bar")))]
                         [:unix+osx (is (= "baz" (f-line "/foo/bar")))]
                         [:osx (is (= "baz" (f-line "/foo/bar")))]
                         [:windows (is (= "baz" (f-line "C:\\foo\\bar")))])))))

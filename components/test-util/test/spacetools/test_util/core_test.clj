(ns spacetools.test-util.core-test
  "Tests of testing utility component."
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.core :refer [filesystem]]
            [clojure.walk :refer [postwalk]]
            [spacetools.test-util.core :refer :all]
            [clojure.string :as str]
            [spacetools.spacedoc.node :as n]))


(st/instrument)


(deftest identity?-fn
  (let [foo #(if (string? %) % 42)]
    (are [f x pred] (pred (identity? f x))
      identity 42 true?
      inc 42 false?
      foo "foo" true?
      foo :foo false?)))


(deftest contains-string?-fn
  (are [s tree pred] (pred (contains-string? s tree))
    "" "" true?
    "" "foo" true?
    "foo" "foo" true?
    "foo" "FOO" true?
    "foo" "foo bar" true?
    "bar" "foo bar baz" true?
    "foo" "fo o" false?
    ".*" "foo" false?
    "foo" [] false?
    "foo" ["foo"] true?
    "bar" ["foo"] false?
    "foo" #{"foo"} true?
    "foo" {:foo "bar"} false?
    "bar" {:a {:b ["foo" {:c "bar"} "baz"]}} true?))


(deftest samples-fn
  (is (pos-int? (samples 1)))
  (is (zero? (rem (samples 3) 3)))
  (is (= (int @gen-mult) (samples 1))))


(deftest make-f-spec-reper-fn
  (is (function? (make-f-spec-reper
                  (s/spec any?)
                  #'spacetools.test-util.core/make-f-spec-reper
                  "foo"))))


(deftest has-n-children?-fn
  (is (thrown? Exception (has-n-children? 42 {:foo {:bar :baz}}))
      "Doesn't work for non SDN map")
  (is (boolean? (has-n-children? 0 (n/text "foo")))
      "Works for SDN nodes")
  (are [node n] (has-n-children? n node)
    (n/text "foo") 0
    (n/todo "foo") 0
    (n/headline "foo" (n/todo "bar")) 1
    (n/headline "foo" (n/headline "bar"
                                  (n/todo "1")
                                  (n/todo "2"))) 1
    (n/headline "foo"
                (n/todo "1")
                (n/todo "2")) 2
    (n/section (n/paragraph (n/text "foo"))
               (n/paragraph (n/text "bar"))) 2
    (n/paragraph (n/text "foo")
                 (n/text "bar")
                 (n/text "baz")) 3))


(deftest count-n?-fn
  (is (thrown? Exception (count-n? 1 "f")) "Doesn't work for strings")
  (are [coll n] (count-n? n coll)
    {:foo :bar} 1
    #{:foo :bar} 2
    ["foo" 42 \d] 3
    `(1 {:foo :bar} "baz" 3) 4))


(deftest create-fs-fn
  (is (every? filesystem? (map (partial create-fs [])
                               [:unix :osx :windows]))))


(deftest filesystem-io
  (testing "Im memory file system (jimfs) I/O [UNIX]"
    (let [fs (create-fs [[:path
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
    (let [fs (create-fs [[:path
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
    (let [fs (create-fs [[:path
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
  (letfn [(f-line [path] (->> path
                              (nio/path filesystem)
                              (nio/read-all-lines)
                              (first)))]
    (is (thrown? Exception
                 (testing-io "foo" [[:foo [:bar "baz"]]]
                             [:bad (is (= "baz" (f-line "/foo/bar")))])))
    (is (testing-io "foo" [[:foo [:bar "baz"]]]
                    [:unix (is (= "baz" (f-line "/foo/bar")))]
                    [:unix+osx (is (= "baz" (f-line "/foo/bar")))]
                    [:osx (is (= "baz" (f-line "/foo/bar")))]
                    [:windows (is (= "baz" (f-line "C:\\foo\\bar")))]))))


(deftest tags->tag->tag-descr-fn
  (are [tags tag->descr] (= tag->descr (tags->tag->tag-descr tags))
    ["foo"] {"foo" "foo tag"}
    ["foo bar"] {"foo bar" "foo bar tag"}
    [] {}
    ["foo" "bar"] {"foo" "foo tag" "bar" "bar tag"}))

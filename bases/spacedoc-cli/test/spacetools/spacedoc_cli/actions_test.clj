(ns spacetools.spacedoc-cli.actions-test
  "Testing cli program actions."
  (:require [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-cli.actions :refer :all]
            [spacetools.spacedoc-io.interface :as sio]
            [spacetools.spacedoc.interface] ;; needed to define nodes.
            [spacetools.test-util.interface :as tu :refer [testing-io]]))


(st/instrument)


(deftest *validate-fn
  (let [good-node (-> :spacetools.spacedoc.node/root
                      (s/exercise 1)
                      ffirst
                      str)
        bad-node {:good :bad}]
    (testing-io "*validate function"
                [[:good.sdn good-node]
                 [:bad.sdn bad-node]
                 [:baz (str good-node good-node)]
                 [:qux [:quux.edn good-node]]
                 [:empty-dir {:type :dir}]]
                [:unix
                 (is (exc/success? (*validate ["/good.sdn"])))
                 (is (exc/success? (*validate ["/qux"])))
                 (is (exc/success? (*validate ["/empty-dir"])))
                 (is (exc/success? (*validate ["/good.sdn" "/qux"])))
                 (is (exc/success? (*validate ["/good.sdn" "/good.sdn"])))
                 (is ((complement str/blank?) @(*validate ["/good.sdn"])))
                 (is (exc/failure? (*validate ["/bad.sdn"])))
                 (is (exc/failure? (*validate ["/good.sdn" "/bad.sdn"])))
                 (is (exc/failure? (*validate ["/"])))]
                [:osx
                 (is (exc/success? (*validate ["/good.sdn"])))
                 (is (exc/success? (*validate ["/qux"])))
                 (is (exc/success? (*validate ["/empty-dir"])))
                 (is (exc/success? (*validate ["/good.sdn" "/qux"])))
                 (is (exc/success? (*validate ["/good.sdn" "/good.sdn"])))
                 (is ((complement str/blank?) @(*validate ["/good.sdn"])))
                 (is (exc/failure? (*validate ["/bad.sdn"])))
                 (is (exc/failure? (*validate ["/good.sdn" "/bad.sdn"])))
                 (is (exc/failure? (*validate ["/"])))]
                [:windows
                 (is (exc/success? (*validate ["C:\\good.sdn"])))
                 (is (exc/success? (*validate ["C:\\qux"])))
                 (is (exc/success? (*validate ["C:\\empty-dir"])))
                 (is (exc/success? (*validate ["C:\\good.sdn" "C:\\qux"])))
                 (is (exc/success? (*validate ["C:\\good.sdn" "C:\\good.sdn"])))
                 (is ((complement str/blank?) @(*validate ["C:\\good.sdn"])))
                 (is (exc/failure? (*validate ["C:\\bad.sdn"])))
                 (is (exc/failure? (*validate ["C:\\good.sdn" "C:\\bad.sdn"])))
                 (is (exc/failure? (*validate ["C:\\"])))])))


(deftest *orgify-fn
  (let [good-node (-> :spacetools.spacedoc.node/root
                      (s/exercise 1)
                      ffirst
                      str)
        bad-node {:good :bad}]
    (testing-io "*orgify function"
                [[:good.sdn good-node]
                 [:bad.sdn bad-node]
                 [:baz (str good-node good-node)]
                 [:qux [:quux.sdn good-node]]
                 [:empty-dir {:type :dir}]]
                [:unix
                 (is (exc/success? (*orgify "/qux" "/foo")))
                 (is (exc/success? (*orgify "/empty-dir" "/new-dir")))
                 (is (exc/success? (*orgify "/empty-dir" "/new-dir")))
                 (is (sio/directory? "/foo"))
                 (is ((complement sio/directory?) "/new-dir"))
                 (is (sio/file? "/foo/quux.org"))
                 (is (seq @(sio/*slurp "/foo/quux.org")))
                 (testing "Testing *orgify function with some bad input files."
                   (is (exc/failure (*orgify "/" "/new-dir"))))]
                [:osx
                 (is (exc/success? (*orgify "/qux" "/foo")))
                 (is (exc/success? (*orgify "/empty-dir" "/new-dir")))
                 (is (exc/success? (*orgify "/empty-dir" "/new-dir")))
                 (is (sio/directory? "/foo"))
                 (is ((complement sio/directory?) "/new-dir"))
                 (is (sio/file? "/foo/quux.org"))
                 (is (seq @(sio/*slurp "/foo/quux.org")))
                 (testing "Testing *orgify function with some bad input files."
                   (is (exc/failure (*orgify "/" "/new-dir"))))]
                [:windows
                 (is (exc/success? (*orgify "C:\\qux" "C:\\foo")))
                 (is (exc/success? (*orgify "C:\\empty-dir" "C:\\new-dir")))
                 (is (exc/success? (*orgify "C:\\empty-dir" "C:\\new-dir")))
                 (is (sio/directory? "C:\\foo"))
                 (is ((complement sio/directory?) "C:\\new-dir"))
                 (is (sio/file? "C:\\foo\\quux.org"))
                 (is (seq @(sio/*slurp "C:\\foo\\quux.org")))
                 (testing "Testing *orgify function with some bad input files."
                   (is (exc/failure (*orgify "C:\\" "C:\\new-dir"))))])))


(deftest *describe-spec-fn
  (let [key-pref ":spacetools.spacedoc-cli.actions-test/"
        test-key-s (str key-pref "foo")]
    (testing "*describe-spec function"
      (is (exc/failure? (*describe-spec (str key-pref "nonexistent-spec"))))
      (s/def ::foo some?)
      (is (exc/success? (*describe-spec test-key-s)))
      (is (exc/failure? (*describe-spec "::foo")))
      (is (str/includes? @(*describe-spec test-key-s) "some?")))))


;; (deftest *relations-fn
;;   (let [good-node (-> :spacetools.spacedoc.node/root
;;                       (s/exercise 1)
;;                       ffirst
;;                       str)
;;         bad-node {:good :bad}]
;;     (testing-io "*relations function"
;;                 [[:good.sdn good-node]
;;                  [:bad.sdn bad-node]
;;                  [:baz (str good-node good-node)]
;;                  [:qux [:quux.sdn good-node]]
;;                  [:empty-dir {:type :dir}]]
;;                 [:unix
;;                  (is (exc/success? (*orgify "/qux" "/foo")))
;;                  (is (exc/success? (*orgify "/empty-dir" "/new-dir")))
;;                  (is (sio/directory? "/foo"))
;;                  (is ((complement sio/directory?) "/new-dir"))
;;                  (is (sio/file? "/foo/quux.org"))
;;                  (is (seq @(sio/*slurp "/foo/quux.org")))]
;;                 [:osx
;;                  ]
;;                 [:windows
;;                  ])))

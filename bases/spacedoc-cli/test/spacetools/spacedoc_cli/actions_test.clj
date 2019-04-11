(ns spacetools.spacedoc-cli.actions-test
  "Testing cli program actions."
  (:require [cats.monad.exception :refer [success? failure?]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.interface :as io :refer [filesystem]]
            [spacetools.spacedoc-cli.actions :refer :all]
            [spacetools.spacedoc.node :as sn] ;; Also needed to define nodes.
            [spacetools.test-util.interface :as tu :refer [testing-io]]))


(st/instrument)


(deftest *validate-fn
  (let [good-node (-> :spacetools.spacedoc.node/root
                      (s/exercise 1)
                      ffirst
                      str)
        bad-node {:bad :node}]
    (testing-io "*validate function"
                [[:good.sdn good-node]
                 [:bad.sdn bad-node]
                 [:double.sdn (str good-node good-node)]
                 [:qux [:quux.edn good-node]]
                 [:empty-dir {:type :dir}]]
                [:unix
                 (is (success? (*validate ["/good.sdn"])))
                 (is (success? (*validate ["/qux"])))
                 (is (success? (*validate ["/empty-dir"])))
                 (is (success? (*validate ["/good.sdn" "/qux"])))
                 (is (success? (*validate ["/good.sdn" "/good.sdn"])))
                 (is ((complement str/blank?) @(*validate ["/good.sdn"])))
                 (is (failure? (*validate ["/bad.sdn"])))
                 (is (failure? (*validate ["/double.sdn"])))
                 (is (failure? (*validate ["/good.sdn" "/bad.sdn"])))
                 (is (failure? (*validate ["/"])))]
                [:osx
                 (is (success? (*validate ["/good.sdn"])))
                 (is (success? (*validate ["/qux"])))
                 (is (success? (*validate ["/empty-dir"])))
                 (is (success? (*validate ["/good.sdn" "/qux"])))
                 (is (success? (*validate ["/good.sdn" "/good.sdn"])))
                 (is ((complement str/blank?) @(*validate ["/good.sdn"])))
                 (is (failure? (*validate ["/bad.sdn"])))
                 (is (failure? (*validate ["/double.sdn"])))
                 (is (failure? (*validate ["/good.sdn" "/bad.sdn"])))
                 (is (failure? (*validate ["/"])))]
                [:windows
                 (is (success? (*validate ["C:\\good.sdn"])))
                 (is (success? (*validate ["C:\\qux"])))
                 (is (success? (*validate ["C:\\empty-dir"])))
                 (is (success? (*validate ["C:\\good.sdn" "C:\\qux"])))
                 (is (success? (*validate ["C:\\good.sdn" "C:\\good.sdn"])))
                 (is ((complement str/blank?) @(*validate ["C:\\good.sdn"])))
                 (is (failure? (*validate ["C:\\bad.sdn"])))
                 (is (failure? (*validate ["C:\\double.sdn"])))
                 (is (failure? (*validate ["C:\\good.sdn" "C:\\bad.sdn"])))
                 (is (failure? (*validate ["C:\\"])))])))


(deftest *orgify-fn
  (let [good-node (-> :spacetools.spacedoc.node/root
                      (s/exercise 1)
                      ffirst
                      str)
        bad-node {:bad :node}]
    (testing-io "*orgify function"
                [[:good.sdn good-node]
                 [:bad.sdn bad-node]
                 [:qux [:quux.sdn good-node]]
                 [:empty-dir {:type :dir}]]
                [:unix
                 (is (success? (*orgify "/qux" "/foo")))
                 (is (success? (*orgify "/empty-dir" "/new-dir")))
                 (is (success? (*orgify "/empty-dir" "/new-dir")))
                 (is (io/directory? "/foo"))
                 (is ((complement io/directory?) "/new-dir"))
                 (is (io/file? "/foo/quux.org"))
                 (is (seq @(io/*slurp "/foo/quux.org")))
                 (testing "Testing *orgify function with some bad input files."
                   (is (failure? (*orgify "/" "/new-dir"))))]
                [:osx
                 (is (success? (*orgify "/qux" "/foo")))
                 (is (success? (*orgify "/empty-dir" "/new-dir")))
                 (is (success? (*orgify "/empty-dir" "/new-dir")))
                 (is (io/directory? "/foo"))
                 (is ((complement io/directory?) "/new-dir"))
                 (is (io/file? "/foo/quux.org"))
                 (is (seq @(io/*slurp "/foo/quux.org")))
                 (testing "Testing *orgify function with some bad input files."
                   (is (failure? (*orgify "/" "/new-dir"))))]
                [:windows
                 (is (success? (*orgify "C:\\qux" "C:\\foo")))
                 (is (success? (*orgify "C:\\empty-dir" "C:\\new-dir")))
                 (is (success? (*orgify "C:\\empty-dir" "C:\\new-dir")))
                 (is (io/directory? "C:\\foo"))
                 (is ((complement io/directory?) "C:\\new-dir"))
                 (is (io/file? "C:\\foo\\quux.org"))
                 (is (seq @(io/*slurp "C:\\foo\\quux.org")))
                 (testing "Testing *orgify function with some bad input files."
                   (is (failure? (*orgify "C:\\" "C:\\new-dir"))))])))


(deftest *describe-spec-fn
  (let [key-pref ":spacetools.spacedoc-cli.actions-test/"
        test-key-s (str key-pref "foo")]
    (testing "*describe-spec function"
      (is (failure? (*describe-spec (str key-pref "nonexistent-spec"))))
      (s/def ::foo some?)
      (is (success? (*describe-spec test-key-s)))
      (is (failure? (*describe-spec "::foo")))
      (is (str/includes? @(*describe-spec test-key-s) "some?")))))


(deftest *relations-fn
  (let [good-node (-> :spacetools.spacedoc.node/root
                      (s/exercise 1)
                      ffirst
                      str)
        bad-node {:bad :node}
        test-node (->> "foobar"
                      sn/todo
                      (sn/root "foo" [])
                      str)]
    (testing-io "*relations function"
                [[:good.sdn good-node]
                 [:test.sdn test-node]
                 [:bad.sdn bad-node]
                 [:qux [:quux.sdn good-node]]
                 [:empty-dir {:type :dir}]]
                [:unix
                 (is (success? (*relations ["/good.sdn"])))
                 (is (success? (*relations ["/good.sdn" "/good.sdn"])))
                 (is (= (*relations ["/good.sdn" "/good.sdn"])
                        (*relations ["/good.sdn"])))
                 (is (success? (*relations ["/qux"])))
                 (is (str/includes? @(*relations ["/test.sdn"]) ":root"))
                 (is (failure? (*relations ["/bad.sdn"])))]
                [:osx
                 (is (success? (*relations ["/good.sdn"])))
                 (is (success? (*relations ["/good.sdn" "/good.sdn"])))
                 (is (= (*relations ["/good.sdn" "/good.sdn"])
                        (*relations ["/good.sdn"])))
                 (is (success? (*relations ["/qux"])))
                 (is (str/includes? @(*relations ["/test.sdn"]) ":root"))
                 (is (failure? (*relations ["/bad.sdn"])))]
                [:windows
                 (is (success? (*relations ["C:\\good.sdn"])))
                 (is (success? (*relations ["C:\\good.sdn" "C:\\good.sdn"])))
                 (is (= (*relations ["C:\\good.sdn" "C:\\good.sdn"])
                        (*relations ["C:\\good.sdn"])))
                 (is (success? (*relations ["C:\\qux"])))
                 (is (str/includes? @(*relations ["C:\\test.sdn"]) ":root"))
                 (is (failure? (*relations ["C:\\bad.sdn"])))])))

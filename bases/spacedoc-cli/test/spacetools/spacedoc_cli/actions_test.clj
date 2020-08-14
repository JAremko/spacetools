(ns spacetools.spacedoc-cli.actions-test
  "Testing cli program actions."
  (:require [cats.monad.exception :refer [failure? success?]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.interface :as io]
            [spacetools.spacedoc-cli.actions :refer :all]
            [spacetools.spacedoc.node :as sn]
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
                [:unix+osx
                 (is ((complement str/blank?) @(*validate ["/good.sdn"])))
                 (are [pred file-paths] (true? (pred (*validate file-paths)))
                   success? ["/good.sdn"]
                   success? ["/qux"]
                   success? ["/empty-dir"]
                   success? ["/good.sdn" "/qux"]
                   success? ["/good.sdn" "/good.sdn"]
                   failure? ["/bad.sdn"]
                   failure? ["/double.sdn"]
                   failure? ["/good.sdn" "/bad.sdn"]
                   failure? ["/"])]
                [:windows
                 (is ((complement str/blank?) @(*validate ["C:\\good.sdn"])))
                 (are [pred file-paths] (true? (pred (*validate file-paths)))
                   success? ["C:\\good.sdn"]
                   success? ["C:\\qux"]
                   success? ["C:\\empty-dir"]
                   success? ["C:\\good.sdn" "C:\\qux"]
                   success? ["C:\\good.sdn" "C:\\good.sdn"]
                   failure? ["C:\\bad.sdn"]
                   failure? ["C:\\double.sdn"]
                   failure? ["C:\\good.sdn" "C:\\bad.sdn"]
                   failure? ["C:\\"])])))


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
                [:unix+osx
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
                       (sn/root "foo" #{})
                       str)]
    (testing-io "*relations function"
                [[:good.sdn good-node]
                 [:test.sdn test-node]
                 [:bad.sdn bad-node]
                 [:qux [:quux.sdn good-node]]
                 [:empty-dir {:type :dir}]]
                [:unix+osx
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

(ns spacetools.spacedoc-cli.args-test
  (:require [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-cli.args :refer :all]
            [spacetools.spacedoc.config :as sc]
            [spacetools.test-util.interface :as tu :refer [testing-io]]))


(st/instrument)


(deftest *parse-fn
  (testing "*parse function."
    (is (exc/success? (*parse [""] [])))
    (is (exc/success? (*parse ["foo"] [])))
    (is (exc/success? (*parse ["foo" "bar"] [])))
    (exc/success? (*parse ["foo" "bar"] [["-b" "--baz" "Bazing."]]))
    (exc/success? (*parse ["-b" "bar"] [["-b" "--baz" "Bazing."]]))
    (exc/success? (*parse ["--baz" "bar"] [["-b" "--baz" "Bazing."]]))
    (exc/failure? (*parse ["-baz" "bar"] [["-b" "--baz" "Bazing."]]))
    (exc/failure? (*parse ["-f" "bar"] [["-b" "--baz" "Bazing."]]))
    (exc/failure? (*parse ["--foo" "bar"] [["-b" "--baz" "Bazing."]]))
    (exc/failure? (*parse ["-foo"] []))
    (testing "Parsing of arguments with a flag."
      (let [{:keys [useless action a-args]}
            @(*parse ["doing" "stuff" "--useless"]
                     [["-u" "--useless" "Doing useless stuff?"]])]
        (is (true? useless))
        (is (= action "doing"))
        (is (= a-args ["stuff"]))))))


(deftest *parse-input-files-fn
  (testing-io "*parse-input-files function"
              [[:foo
                [:bar.sdn]
                [:baz.edn]
                [:qux
                 [:qux.sdn]]]
               [:qux.sdn]
               [:quux.edn]
               [:quuz {:type :dir}]]
              [:unix
               (is (exc/success? (*parse-input-files ["/"])))
               (is (exc/success? (*parse-input-files ["/qux.sdn"])))
               (is (exc/success? (*parse-input-files ["/" "/qux.sdn"])))
               (is (exc/success? (*parse-input-files ["/quuz"])))
               (is (exc/failure? (*parse-input-files ["/quux.edn"])))
               (is (exc/failure? (*parse-input-files ["/quux"])))
               (is (= #{"/qux.sdn"
                        "/foo/bar.sdn"
                        "/foo/qux/qux.sdn"}
                      @(*parse-input-files ["/"])))]
              [:osx
               (is (exc/success? (*parse-input-files ["/"])))
               (is (exc/success? (*parse-input-files ["/qux.sdn"])))
               (is (exc/success? (*parse-input-files ["/" "/qux.sdn"])))
               (is (exc/success? (*parse-input-files ["/quuz"])))
               (is (exc/failure? (*parse-input-files ["/quux.edn"])))
               (is (exc/failure? (*parse-input-files ["/quux"])))
               (is (= #{"/qux.sdn"
                        "/foo/bar.sdn"
                        "/foo/qux/qux.sdn"}
                      @(*parse-input-files ["/"])))]
              [:windows
               (is (exc/success? (*parse-input-files ["C:\\"])))
               (is (exc/success? (*parse-input-files ["C:\\qux.sdn"])))
               (is (exc/success? (*parse-input-files ["C:\\" "C:\\qux.sdn"])))
               (is (exc/success? (*parse-input-files ["C:\\quuz"])))
               (is (exc/failure? (*parse-input-files ["C:\\quux.edn"])))
               (is (exc/failure? (*parse-input-files ["C:\\quux"])))
               (is (= #{"C:\\qux.sdn"
                        "C:\\foo\\bar.sdn"
                        "C:\\foo\\qux\\qux.sdn"}
                      @(*parse-input-files ["C:\\"])))]))


;; (deftest *parse-input-files-fn
;;   (let [root-node (ffirst (s/exercise :spacetools.spacedoc.node/root 1))]
;;     (testing-io "*parse-input-files function"
;;                 [[:foo
;;                   [:bar.sdn (str {:foo :bar})]
;;                   [:baz.sdn (str root-node root-node)]
;;                   [:qux
;;                    [:qux.sdn (str root-node)]]]
;;                  [:qux.sdn]
;;                  [:quux.edn]]
;;                 [:unix
;;                  (is (exc/failure? (*parse-input-files ["/"])))
;;                  ]
;;                 [:osx
;;                  (is (exc/failure? (*parse-input-files ["/"])))
;;                  ]
;;                 [:windows
;;                  (is (exc/failure? (*parse-input-files ["C:\\"])))
;;                  ])))


;; (deftest *confibure-fn
;;   (testing-io "*configure function" []
;;               [:unix
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:osx
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:windows
;;                (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))

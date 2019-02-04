(ns spacetools.spacedoc-cli.args-test
  (:require [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-cli.args :as args]
            [spacetools.spacedoc.config :as sc]
            [spacetools.test-util.interface :as tu :refer [testing-io]]))


(st/instrument)


(deftest *parse-fn
  (testing "*parse function."
    (is (exc/success? (args/*parse [""] [])))
    (is (exc/success? (args/*parse ["foo"] [])))
    (is (exc/success? (args/*parse ["foo" "bar"] [])))
    (exc/success? (args/*parse ["foo" "bar"]
                               [["-b" "--baz" "Bazing."]]))
    (exc/success? (args/*parse ["-b" "bar"]
                               [["-b" "--baz" "Bazing."]]))
    (exc/success? (args/*parse ["--baz" "bar"]
                               [["-b" "--baz" "Bazing."]]))
    (exc/failure? (args/*parse ["-baz" "bar"]
                               [["-b" "--baz" "Bazing."]]))
    (exc/failure? (args/*parse ["-f" "bar"]
                               [["-b" "--baz" "Bazing."]]))
    (exc/failure? (args/*parse ["--foo" "bar"]
                               [["-b" "--baz" "Bazing."]]))
    (exc/failure? (args/*parse ["-foo"] []))
    (testing "Parsing of arguments with a flag."
      (let [{:keys [useless action a-args]}
            @(args/*parse ["doing" "stuff" "--useless"]
                          [["-u" "--useless" "Doing useless stuff?"]])]
        (is (true? useless))
        (is (= action "doing"))
        (is (= a-args ["stuff"]))))))


;; (deftest *parse-input-files-fn
;;   (testing-io "*parse-input-files function" []
;;               [:unix
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:osx
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:windows
;;                (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))


;; (deftest *confibure-fn
;;   (testing-io "*configure function" []
;;               [:unix
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:osx
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:windows
;;                (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))

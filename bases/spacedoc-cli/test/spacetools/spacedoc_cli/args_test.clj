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

;; (args/*parse ["d"] [])


(deftest *parse-fn
  (testing "*parse function"
    (is (exc/success? (args/*parse [""] [])))))

;; (deftest *parse-input-files
;;   (testing-io "" []
;;               [:unix
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:osx
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:windows
;;                (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))


;; (deftest *confibure-fn
;;   (testing-io "" []
;;               [:unix
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:osx
;;                (is (= "/work/bar" (str (io/absolute "bar"))))]
;;               [:windows
;;                (is (= "C:\\work\\bar" (str (io/absolute "bar"))))]))

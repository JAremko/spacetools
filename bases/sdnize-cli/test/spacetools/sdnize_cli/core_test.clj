(ns spacetools.sdnize-cli.core-test
  (:require [clojure.test :refer :all]
            [spacetools.sdnize-cli.core :as core]))

;; Add tests here...
(deftest hello-world-example-test
  (let [output (with-out-str (core/-main))]
    (is (= "Hello world!\n"
           output))))

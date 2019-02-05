(ns spacetools.spacedoc-cli.args-test
  "Testing parsing of the program inputs."
  (:require [cats.monad.exception :as exc]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-cli.args :refer :all]
            [spacetools.spacedoc-io.interface :refer [file?]]
            [spacetools.spacedoc.config :as sc]
            [spacetools.test-util.interface :refer [testing-io]]))


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


(deftest *configure!-fn
  (let [test-key ::testing-overrides
        overrided (assoc sc/default-config test-key :test-val)]
    (testing-io "*configure! function"
                [[:defaults.edn (str sc/default-config)]
                 [:empty-overrides.edn (str {})]
                 [:overrided.edn (str overrided)]
                 [:bad-overrides.edn "foo"]]
                [:unix
                 (testing "Sanity"
                   (is (file? "/defaults.edn"))
                   (is (file? "/empty-overrides.edn"))
                   (is (file? "/bad-overrides.edn"))
                   (is (not (file? "/nonexistent.edn"))))
                 (is (exc/success? (*configure! "/defaults.edn")))
                 (is (exc/success? (*configure! "/empty-overrides.edn")))
                 (is (exc/success? (*configure! "/nonexistent.edn")))
                 (is (exc/failure? (*configure! "/bad-overrides.edn")))
                 (is (= @(*configure! "/empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "/defaults.edn") @sc/*configs))
                 (is (= @(*configure! "/nonexistent.edn") @sc/*configs))
                 (is (and (= @(*configure! "/overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))]
                [:osx
                 (testing "Sanity"
                   (is (file? "/defaults.edn"))
                   (is (file? "/empty-overrides.edn"))
                   (is (file? "/bad-overrides.edn"))
                   (is (not (file? "/nonexistent.edn"))))
                 (is (exc/success? (*configure! "/defaults.edn")))
                 (is (exc/success? (*configure! "/empty-overrides.edn")))
                 (is (exc/success? (*configure! "/nonexistent.edn")))
                 (is (exc/failure? (*configure! "/bad-overrides.edn")))
                 (is (= @(*configure! "/empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "/defaults.edn") @sc/*configs))
                 (is (= @(*configure! "/nonexistent.edn") @sc/*configs))
                 (is (and (= @(*configure! "/overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))]
                [:windows
                 (testing "Sanity"
                   (is (file? "C:\\defaults.edn"))
                   (is (file? "C:\\empty-overrides.edn"))
                   (is (file? "C:\\bad-overrides.edn"))
                   (is (not (file? "C:\\nonexistent.edn"))))
                 (is (exc/success? (*configure! "C:\\defaults.edn")))
                 (is (exc/success? (*configure! "C:\\empty-overrides.edn")))
                 (is (exc/success? (*configure! "C:\\nonexistent.edn")))
                 (is (exc/failure? (*configure! "C:\\bad-overrides.edn")))
                 (is (= @(*configure! "C:\\empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "C:\\defaults.edn") @sc/*configs))
                 (is (= @(*configure! "C:\\nonexistent.edn") @sc/*configs))
                 (is (and (= @(*configure! "C:\\overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))])))

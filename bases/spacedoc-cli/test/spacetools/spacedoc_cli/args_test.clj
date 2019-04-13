(ns spacetools.spacedoc-cli.args-test
  "Testing parsing of the cli program inputs."
  (:require [cats.monad.exception :refer [success? failure?]]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.interface :refer [file? filesystem] :as io]
            [spacetools.spacedoc-cli.args :refer :all]
            [spacetools.spacedoc.config :as sc]
            [spacetools.test-util.interface :refer [testing-io]]
            [nio2.core :as nio]))


(st/instrument)


(deftest *parse-fn
  (testing "*parse function."
    (is (success? (*parse [""] [])))
    (is (success? (*parse ["foo"] [])))
    (is (success? (*parse ["foo" "bar"] [])))
    (success? (*parse ["foo" "bar"] [["-b" "--baz" "Bazing."]]))
    (success? (*parse ["-b" "bar"] [["-b" "--baz" "Bazing."]]))
    (success? (*parse ["--baz" "bar"] [["-b" "--baz" "Bazing."]]))
    (failure? (*parse ["-baz" "bar"] [["-b" "--baz" "Bazing."]]))
    (failure? (*parse ["-f" "bar"] [["-b" "--baz" "Bazing."]]))
    (failure? (*parse ["--foo" "bar"] [["-b" "--baz" "Bazing."]]))
    (failure? (*parse ["-foo"] []))
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
               (is (success? (*parse-input-files ["/"])))
               (is (success? (*parse-input-files ["/qux.sdn"])))
               (is (success? (*parse-input-files ["/" "/qux.sdn"])))
               (is (success? (*parse-input-files ["/quuz"])))
               (is (failure? (*parse-input-files ["/quux.edn"])))
               (is (failure? (*parse-input-files ["/quux"])))
               (is (= #{"/qux.sdn"
                        "/foo/bar.sdn"
                        "/foo/qux/qux.sdn"}
                      @(*parse-input-files ["/"])))]
              [:osx
               (is (success? (*parse-input-files ["/"])))
               (is (success? (*parse-input-files ["/qux.sdn"])))
               (is (success? (*parse-input-files ["/" "/qux.sdn"])))
               (is (success? (*parse-input-files ["/quuz"])))
               (is (failure? (*parse-input-files ["/quux.edn"])))
               (is (failure? (*parse-input-files ["/quux"])))
               (is (= #{"/qux.sdn"
                        "/foo/bar.sdn"
                        "/foo/qux/qux.sdn"}
                      @(*parse-input-files ["/"])))]
              [:windows
               (is (success? (*parse-input-files ["C:\\"])))
               (is (success? (*parse-input-files ["C:\\qux.sdn"])))
               (is (success? (*parse-input-files ["C:\\" "C:\\qux.sdn"])))
               (is (success? (*parse-input-files ["C:\\quuz"])))
               (is (failure? (*parse-input-files ["C:\\quux.edn"])))
               (is (failure? (*parse-input-files ["C:\\quux"])))
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
                 (is (success? (*configure! nil)))
                 (is (success? (*configure! "/defaults.edn")))
                 (is (success? (*configure! "/empty-overrides.edn")))
                 (is (failure? (*configure! "/nonexistent.edn")))
                 (is (failure? (*configure! "/bad-overrides.edn")))
                 (is (= @(*configure! nil) @sc/*configs))
                 (is (= @(*configure! "/empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "/defaults.edn") @sc/*configs))
                 (is (and (= @(*configure! "/overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))]
                [:osx
                 (testing "Sanity"
                   (is (file? "/defaults.edn"))
                   (is (file? "/empty-overrides.edn"))
                   (is (file? "/bad-overrides.edn"))
                   (is (not (file? "/nonexistent.edn"))))
                 (is (success? (*configure! nil)))
                 (is (success? (*configure! "/defaults.edn")))
                 (is (success? (*configure! "/empty-overrides.edn")))
                 (is (failure? (*configure! "/nonexistent.edn")))
                 (is (failure? (*configure! "/bad-overrides.edn")))
                 (is (= @(*configure! nil) @sc/*configs))
                 (is (= @(*configure! "/empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "/defaults.edn") @sc/*configs))
                 (is (and (= @(*configure! "/overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))]
                [:windows
                 (testing "Sanity"
                   (is (file? "C:\\defaults.edn"))
                   (is (file? "C:\\empty-overrides.edn"))
                   (is (file? "C:\\bad-overrides.edn"))
                   (is (not (file? "C:\\nonexistent.edn"))))
                 (is (success? (*configure! nil)))
                 (is (success? (*configure! "C:\\defaults.edn")))
                 (is (success? (*configure! "C:\\empty-overrides.edn")))
                 (is (failure? (*configure! "C:\\nonexistent.edn")))
                 (is (failure? (*configure! "C:\\bad-overrides.edn")))
                 (is (= @(*configure! nil) @sc/*configs))
                 (is (= @(*configure! "C:\\empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "C:\\defaults.edn") @sc/*configs))
                 (is (and (= @(*configure! "C:\\overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))])))

(ns spacetools.spacedoc-cli.args-test
  "Testing parsing of the cli program inputs."
  (:require [cats.monad.exception :refer [failure? success?]]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.interface :as io :refer [file?]]
            [spacetools.spacedoc-cli.args :refer :all]
            [spacetools.spacedoc.config :as sc]
            [spacetools.test-util.interface :refer [testing-io]]))


(st/instrument)


(deftest *parse-fn
  (testing "*parse function."
    (are [pred args ops] (true? (pred (*parse args ops)))
      success? [""] []
      success? ["foo"] []
      success? ["foo" "bar"] []
      success? ["foo" "bar"] [["-b" "--baz" "Bazing."]]
      success? ["-b" "bar"] [["-b" "--baz" "Bazing."]]
      success? ["--baz" "bar"] [["-b" "--baz" "Bazing."]]
      failure? ["-baz" "bar"] [["-b" "--baz" "Bazing."]]
      failure? ["-f" "bar"] [["-b" "--baz" "Bazing."]]
      failure? ["--foo" "bar"] [["-b" "--baz" "Bazing."]]
      failure? ["-foo"] [])
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
              [:unix+osx
               (are [pred files] (true? (pred (*parse-input-files files)))
                 success? ["/"]
                 success? ["/qux.sdn"]
                 success? ["/" "/qux.sdn"]
                 success? ["/quuz"]
                 failure? ["/quux.edn"]
                 failure? ["/quux"])
               (is (= #{"/qux.sdn"
                        "/foo/bar.sdn"
                        "/foo/qux/qux.sdn"}
                      @(*parse-input-files ["/"])))]
              [:windows
               (are [pred files] (true? (pred (*parse-input-files files)))
                 success? ["C:\\"]
                 success? ["C:\\qux.sdn"]
                 success? ["C:\\" "C:\\qux.sdn"]
                 success? ["C:\\quuz"]
                 failure? ["C:\\quux.edn"]
                 failure? ["C:\\quux"])
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
                [:unix+osx
                 (testing "Sanity"
                   (are [pred x] (pred (file? x))
                     true? "/defaults.edn"
                     true? "/empty-overrides.edn"
                     true? "/bad-overrides.edn"
                     false? "/nonexistent.edn"))
                 (are [pred file-path] (true? (pred (*configure! file-path)))
                   success? nil
                   success? "/defaults.edn"
                   success? "/empty-overrides.edn"
                   failure? "/nonexistent.edn"
                   failure? "/bad-overrides.edn")
                 (is (= @(*configure! nil) @sc/*configs))
                 (is (= @(*configure! "/empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "/defaults.edn") @sc/*configs))
                 (is (and (= @(*configure! "/overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))]
                [:windows
                 (testing "Sanity"
                   (are [pred x] (pred (file? x))
                     true? "C:\\defaults.edn"
                     true? "C:\\empty-overrides.edn"
                     true? "C:\\bad-overrides.edn"
                     false? "C:\\nonexistent.edn"))
                 (are [pred file-path] (true? (pred (*configure! file-path)))
                   success? nil
                   success? "C:\\defaults.edn"
                   success? "C:\\empty-overrides.edn"
                   failure? "C:\\nonexistent.edn"
                   failure? "C:\\bad-overrides.edn")
                 (is (= @(*configure! nil) @sc/*configs))
                 (is (= @(*configure! "C:\\empty-overrides.edn") @sc/*configs))
                 (is (= @(*configure! "C:\\defaults.edn") @sc/*configs))
                 (is (and (= @(*configure! "C:\\overrided.edn") @sc/*configs)
                          (test-key @sc/*configs)))])))

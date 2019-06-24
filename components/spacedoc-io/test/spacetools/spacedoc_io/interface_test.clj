(ns spacetools.spacedoc-io.interface-test
  "Testing interface of the `spacedoc-io` component."
  (:require [cats.monad.exception :refer [success? failure? exception?]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nio2.core :as nio]
            [orchestra.spec.test :as st]
            [spacetools.fs-io.interface :refer [filesystem]]
            [spacetools.spacedoc-io.interface :as sio]
            [spacetools.spacedoc.interface :refer [default-config]]
            [spacetools.spacedoc.node :as sn]
            [spacetools.test-util.interface :refer [testing-io]]))


(st/instrument)


(deftest *fp->sdn-fn
  (let [valid-sdn (->> (sn/todo "foo")
                       (sn/root "bar" #{})
                       (str))]
    (testing-io "*fp->sdn function" [[:foo.sdn valid-sdn]
                                     [:bar.sdn "{:foo :bar}"]
                                     [:baz.txt valid-sdn]
                                     [:qux.sdn (str valid-sdn valid-sdn)]]
                [:unix+osx
                 (is (success? (sio/*fp->sdn "/foo.sdn")))
                 (is (success? (sio/*fp->sdn "/baz.txt")))
                 (is (= valid-sdn (str @(sio/*fp->sdn "/baz.txt"))))
                 (is (failure? (sio/*fp->sdn "/qux.sdn")))
                 (is (failure? (sio/*fp->sdn "/")))
                 (is (failure? (sio/*fp->sdn "/bar.sdn")))]
                [:windows
                 (is (success? (sio/*fp->sdn "C:\\foo.sdn")))
                 (is (success? (sio/*fp->sdn "C:\\baz.txt")))
                 (is (= valid-sdn (str @(sio/*fp->sdn "C:\\baz.txt"))))
                 (is (failure? (sio/*fp->sdn "C:\\qux.sdn")))
                 (is (failure? (sio/*fp->sdn "C:\\")))
                 (is (failure? (sio/*fp->sdn "C:\\bar.sdn")))])))


(deftest *read-cfg-overrides-fn
  (testing-io "*read-cfg-overrides function" [[:foo.edn (str default-config)]
                                              [:bar.edn "{:foo :bar}"]]
              [:unix+osx
               (is (success? (sio/*read-cfg-overrides "/foo.edn")))
               (is (s/valid? :spacetools.spacedoc.config/overriding-configs
                             @(sio/*read-cfg-overrides "/foo.edn")))
               (is (exception? (sio/*read-cfg-overrides "/")))
               (is (exception? (sio/*read-cfg-overrides "/bar.edn")))]
              [:windows
               (is (success? (sio/*read-cfg-overrides "C:\\foo.edn")))
               (is (s/valid? :spacetools.spacedoc.config/overriding-configs
                             @(sio/*read-cfg-overrides "C:\\foo.edn")))
               (is (exception? (sio/*read-cfg-overrides "C:\\")))
               (is (exception? (sio/*read-cfg-overrides "C:\\bar.edn")))]))

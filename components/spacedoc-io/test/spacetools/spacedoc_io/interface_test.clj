(ns spacetools.spacedoc-io.interface-test
  "Testing interface of the `spacedoc-io` component."
  (:require [cats.monad.exception :refer [exception? failure? success?]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
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
                 (are [pred file-path] (true? (pred (sio/*fp->sdn file-path)))
                   success? "/foo.sdn"
                   success? "/baz.txt"
                   failure? "/qux.sdn"
                   failure? "/"
                   failure? "/bar.sdn")
                 (is (= valid-sdn (str @(sio/*fp->sdn "/baz.txt"))))]
                [:windows
                 (are [pred file-path] (true? (pred (sio/*fp->sdn file-path)))
                   success? "C:\\foo.sdn"
                   success? "C:\\baz.txt"
                   failure? "C:\\qux.sdn"
                   failure? "C:\\"
                   failure? "C:\\bar.sdn")
                 (is (= valid-sdn (str @(sio/*fp->sdn "C:\\baz.txt"))))])))


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

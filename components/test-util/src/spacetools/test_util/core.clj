(ns spacetools.test-util.core
  "Shared defs for tests"
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [report]]
            [clojure.test.check.clojure-test :refer [default-reporter-fn]]
            [environ.core :refer [env]]
            [orchestra.core :refer [defn-spec]]))


(def gen-mult (delay
               (let [g-m (or (when-let [g-m-str (env :gentest-multiplier)]
                               (let [g-m (read-string g-m-str)]
                                 (when (pos? g-m) g-m)))
                             1)]
                 (prn (format "Gentest sample's count multiplier is (%s)" g-m))
                 g-m)))


(defn-spec samples nat-int?
  "Multiplies BASE-SAMPLE-COUNT by `gen-mult` and returns it as `pos-int?`."
  [base-sample-count nat-int?]
  (max 1 (int (* @gen-mult base-sample-count))))


(defn-spec function? boolean?
  [f any?]
  (= (type f) clojure.lang.IFn))


(defn-spec make-f-spec-reper function?
  "Like `default-reporter-fn` but for spec reports."
  [ret-spec s/spec? f qualified-symbol? f-name string?]
  (fn spec-rep-fn
    [{type :type [fn-args] :smallest :as args}]
    (case type
      :failure (let [ret-val (f fn-args)]
                 (report (->> (s/explain-data ret-spec ret-val)
                              (:clojure.spec.alpha/problems)
                              (hash-map :problems)
                              (merge {:f-name f-name
                                      :f-args (first (:fail args))
                                      :f-val ret-val}))))
      :complete (default-reporter-fn args)
      nil)))

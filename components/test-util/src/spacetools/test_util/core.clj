(ns spacetools.test-util.core
  "Shared defs for tests"
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [report]]
            [clojure.test.check.clojure-test :refer [default-reporter-fn]]
            [environ.core :refer [env]]
            [orchestra.core :refer [defn-spec]])
  (:import  [com.google.common.jimfs Configuration Jimfs]))


(defn-spec filesystem? boolean?
  [fs any?]
  (instance? com.google.common.jimfs.JimfsFileSystem fs))


(defn new-fs
  []

  )


(Jimfs/newFileSystem (Configuration/unix))

(def gen-mult (delay
               (let [g-m (or (when-let [g-m-str (env :gentest-multiplier)]
                               (let [g-m (read-string g-m-str)]
                                 (when (pos? g-m) g-m)))
                             1)]
                 (prn (format "Gentest sample's count multiplier is (%s)" g-m))
                 g-m)))


(defn samples
  "Multiplies BASE-SAMPLE-COUNT by `gen-mult` and returns it as `pos-int?`."
  [base-sample-count]
  (max 1 (int (* @gen-mult base-sample-count))))


(defn make-f-spec-reper
  "Like `default-reporter-fn` but for spec reports."
  [ret-spec f f-name]
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

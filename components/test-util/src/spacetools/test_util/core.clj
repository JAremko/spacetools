(ns spacetools.test-util.core
  "Shared utility for testing stuff."
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [report]]
            [clojure.test.check.clojure-test :refer [default-reporter-fn]]
            [environ.core :refer [env]]
            [nio2.core :as nio2]
            [orchestra.core :refer [defn-spec]])
  (:import  [com.google.common.jimfs Jimfs Configuration]))


(def gen-mult (delay
               (let [g-m (or (when-let [g-m-str (env :gentest-multiplier)]
                               (let [g-m (read-string g-m-str)]
                                 (when (pos? g-m) g-m)))
                             1)]
                 (prn (format "Gentest sample's count multiplier is (%s)" g-m))
                 g-m)))


(defn-spec samples pos-int?
  "Multiplies BASE-SAMPLE-COUNT by `gen-mult` and returns it as `pos-int?`."
  [base-sample-count pos-int?]
  (max 1 (int (* @gen-mult base-sample-count))))


(defn-spec make-f-spec-reper fn?
  "Like `default-reporter-fn` but for spec reports."
  [ret-spec s/spec? f fn? f-name qualified-ident?]
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


(defn-spec filesystem? boolean?
  [f-sys any?]
  (instance? com.google.common.jimfs.JimfsFileSystem f-sys))


;;; straight from https://github.com/potetm/nio2/blob/master/test/nio2/jimfs.clj

(defprotocol IConfiguration
  (os [_])
  (init-fs [_])
  (fs-root [_]))


(defrecord OsConfiguration [os init-config rootfs]
  IConfiguration
  (os [_]
    os)
  (init-fs [_]
    (Jimfs/newFileSystem (init-config)))
  (fs-root [_]
    rootfs))


(def configurations
  {:unix (->OsConfiguration :unix #(Configuration/unix) "/")
   :osx (->OsConfiguration :osx #(Configuration/osX) "/")
   :windows (->OsConfiguration :windows #(Configuration/windows) "C:\\")})


(defn-spec create-fs filesystem?
  ([struct vector?]
   (create-fs struct :unix))
  ([struct vector? os-kw keyword?]
   (let [config (os-kw configurations)
         fs (init-fs config)]
     (nio2/create-fs-tree! fs (fs-root config) struct)
     fs)))

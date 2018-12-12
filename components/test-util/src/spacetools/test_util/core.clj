(ns spacetools.test-util.core
  "Shared utility for testing stuff."
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [report]]
            [clojure.test.check.clojure-test :refer [default-reporter-fn]]
            [environ.core :refer [env]]
            [nio2.core :as nio]
            [orchestra.core :refer [defn-spec]])
  (:import  [com.google.common.jimfs Jimfs Configuration]))


(def gen-mult
  "Multiplier for generative testing."
  (delay
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
  [ret-spec s/spec? f var? f-name string?]
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
  "Return true if X is a filesystem."
  [x any?]
  (instance? com.google.common.jimfs.JimfsFileSystem x))


(defprotocol IConfiguration
  (os [_])
  (init-fs [_])
  (fs-root [_]))


(defrecord OsConfiguration [os init-config rootfs]
  IConfiguration
  (os [_]
    os)
  (init-fs [_]
    (Jimfs/newFileSystem ^Configuration (init-config)))
  (fs-root [_]
    rootfs))


(def configurations
  {:unix (->OsConfiguration :unix #(Configuration/unix) "/")
   :osx (->OsConfiguration :osx #(Configuration/osX) "/")
   :windows (->OsConfiguration :windows #(Configuration/windows) "C:\\")})


(defn-spec create-fs filesystem?
  "Create in-memory filesystem.
STRUCT is a recursive vector of files/directories.
OS-KW is a keyword specifying OS family: `:unix`(default), `:osx`, `:windows`."
  ([struct vector?]
   (create-fs struct :unix))
  ([struct vector? os-kw keyword?]
   (let [config (os-kw configurations)
         fs (init-fs config)]
     (nio/create-fs-tree! fs (fs-root config) struct)
     fs)))

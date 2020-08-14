(ns spacetools.test-util.core
  "Shared utility for testing."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer [report testing]]
            [clojure.test.check.clojure-test :refer [default-reporter-fn]]
            [environ.core :refer [env]]
            [nio2.core :as nio]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface :as sio]
            [spacetools.spacedoc.interface :as sd])
  (:import [com.google.common.jimfs Configuration Jimfs]))


(defn-spec contains-string? boolean?
  "Returns true if TREE contains S string at any level."
  [s string? tree seqable?]
  (->> tree
       (tree-seq (every-pred seqable? (complement string?))
                 seq)
       (some (every-pred string?
                         #(str/includes? (str/lower-case %)
                                         (str/lower-case s))))
       some?))


(defn-spec has-n-children? boolean?
  "Returns true if SDN NODE has N number of children."
  [n nat-int? node (s/nilable sd/node?)]
  (= n (count (:children node))))


(defn-spec count-n? boolean?
  "Returns true if COLL collection's  `count` is N."
  [n nat-int? coll coll?]
  (= n (count coll)))


(def gen-mult
  "Multiplier for generative testing."
  (delay
    (let [g-m (max ((fnil read-string "0") (env :gentest-multiplier)) 1)]
      (prn (format "Gentest sample's count multiplier is (%s)" g-m))
      g-m)))


(defn-spec samples pos-int?
  "Multiplies BASE-SAMPLE-COUNT by :gentest-multiplier."
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
  (instance? java.nio.file.FileSystem x))


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
  " Operation system specific configurations."
  {:unix (->OsConfiguration :unix #(Configuration/unix) "/")
   :osx (->OsConfiguration :osx #(Configuration/osX) "/")
   :windows (->OsConfiguration :windows #(Configuration/windows) "C:\\")})


(defn-spec create-fs filesystem?
  "Create in-memory filesystem.
STRUCT is a recursive vector of files/directories.
For example: [[:path [:to [:file-a] [:file-b]]]].
OS-KW is a keyword specifying OS family: `:unix`(default), `:osx`, `:windows`."
  ([struct vector?]
   (create-fs struct :unix))
  ([struct vector? os-kw keyword?]
   (let [config (os-kw configurations)
         fs (init-fs config)]
     (nio/create-fs-tree! fs (fs-root config) struct)
     fs)))


(s/fdef testing-io
  :args (s/cat :name (s/and string? seq)
               :struct vector?
               :test-forms (s/+ seq))
  :ret seq)


(defmacro testing-io
  "Testing IO functions with fresh \"in-memory\" file-system.
  NAME is the name of the test (goes to `testing`).
  STRUCT is the file system initial structure - see `tu/create-fs`.
  Each element of TEST-FORMS has the shape: [OS BODY] where:
  OS is the operation system used in the test and mast be one of the
  keywords: `:unix` `:osx` `:windows` `:unix+osx` - the last one runs
  tests both for osx and unix file systems.
  BODY is the `testing` macro body."
  [name struct & test-forms]
  (conj (for [test-form test-forms
              :let [[os-key & body] test-form]
              os (if (= :unix+osx os-key)
                   [:unix :osx]
                   [os-key])]
          `(testing (format "Testing %s with in-memory %s filesystem"
                            ~name
                            (str/capitalize (name ~os)))
             (let [fs# (create-fs ~struct ~os)]
               (with-redefs [spacetools.fs-io.core/filesystem fs#
                             spacetools.fs-io.interface/filesystem fs#]
                 ~@body))))
        'do))

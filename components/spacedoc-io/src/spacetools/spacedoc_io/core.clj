(ns spacetools.spacedoc-io.core
  "File-system I/O module.
  Functions that can fail return `cats.monad.exception`.
  Actually I/O wrapped in `io!`."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [nio2.core :as nio]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.interface :as sdu])
  (:import (java.nio.file Path)
           (java.io File)))


(def filesystem (nio/default-fs))


(defn-spec path? boolean?
  [path any?]
  (instance? Path path))


(defn-spec file-or-dir? boolean?
  [file any?]
  (instance? File file))


(defn-spec file-ref? boolean?
  [f-ref any?]
  ((some-fn string? path? file-or-dir?) f-ref))


(defn-spec str->path path?
  [s string?]
  (nio/path filesystem s))


(defn-spec any->path (s/nilable path?)
  [f-ref file-ref?]
  ((condp #(%1 %2) f-ref
     string? str->path
     file-or-dir? (comp str->path str)
     path? identity
     (constantly nil))
   f-ref))


(defn-spec directory? boolean?
  [file any?]
  (io! (some-> file (any->path) (nio/dir?))))


(defn-spec file? boolean?
  [file any?]
  (io! (some-> file (any->path) (nio/file?))))


(defn-spec error? boolean?
  [err any?]
  (instance? Throwable err))


(defn-spec regexp? boolean?
  [re-pat any?]
  (instance? java.util.regex.Pattern re-pat))


(defn-spec file-with-ext? boolean?
  [ext-pat regexp? path file-ref?]
  (when-let [fp (any->path path)]
    (and (nio/file? fp)
         (some->> fp (str) (re-matches ext-pat) (some?)))))


(defn-spec sdn-file? boolean?
  [path file-ref?]
  (file-with-ext? #"(?i).*\.sdn$" path))


(defn-spec edn-file? boolean?
  [path file-ref?]
  (file-with-ext? #"(?i).*\.edn$" path))


(defn-spec absolute path?
  [path file-ref?]
  (io! (-> path
           (any->path)
           (nio/file)
           (.getAbsoluteFile)
           (.getCanonicalPath)
           (str->path))))


(defn-spec rebase-path path?
  [old-base file-ref? new-base file-ref? path file-ref?]
  (io! (let [[a-ob a-nb a-p] (map (comp str absolute any->path)
                                  [old-base new-base path])]
         (nio/file (str->path (str/replace-first a-p a-ob a-nb))))))


(defn-spec exception-of? boolean?
  "Returns true if `exc/failure` or `exc/success` wraps value satisfying pred."
  [pred fn?]
  (fn [*val]
    (and (exc/exception? *val)
         (if (exc/success? *val)
           (m/bind *val pred)
           true))))


(defn-spec *fp->sdn (exception-of? map?)
  "Read and validate .SDN file."
  ([path file-ref?]
   (*fp->sdn :spacetools.spacedoc.node/root path))
  ([root-node-spec s/spec? path file-ref?]
   (io! (exc/try-or-recover
         (with-open [input (->> path
                                any->path
                                (nio/buffered-reader)
                                (io/reader)
                                (java.io.PushbackReader.))]
           (let [[obj fin] (repeatedly 2 (partial edn/read {:eof :fin} input))]
             (cond
               (not= :fin fin)
               (throw (Exception. ".SDN file should contain single root form."))
               (not= :root (:tag obj))
               (throw (Exception. "Non-root top level node in .SDN file."))
               ((complement s/valid?) root-node-spec obj)
               (throw (ex-info "Validation filed." (sdu/explain-deepest obj)))
               :else obj)))
         (fn [^Exception err]
           (exc/failure
            (ex-info (.getMessage err) (if-let [ed (ex-data err)]
                                         (assoc ed :file path)
                                         {:file path}))))))))


(defn-spec output-err nat-int?
  [& msg (s/* string?)]
  (io! (binding [*out* *err*]
         (println (str/join msg))
         2)))


(defn-spec output-ok nat-int?
  [& msg (s/* string?)]
  (io! (println (str/join msg))
       0))


(defn-spec err->msg string?
  [^Throwable err error?]
  (let [{:keys [cause data]} (Throwable->map err)]
    (str/join \newline
              (interleave
               (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                    ["Cause" "File" "Data"])
               [cause
                (or (:file data) "<none>")
                (or (:problems data) (seq data) "<none>")]))))


(defn-spec try-m->output nil?
  "Prints output to stderr or stdout and `System/exit` with code 0 or 2"
  [*output (exception-of? any?)]
  (io! (System/exit
        (let [output (m/extract *output)]
          (if (exc/failure? *output)
            (output-err
             (format "Error:\n%s\nRun with \"-h\" for usage" (err->msg output)))
            (output-ok (str output)))))))


(defn-spec *read-cfg-overrides (exception-of? map?)
  [path file-ref?]
  (io! (exc/try-or-recover
        (when (edn-file? path)
          (with-open [input (->> path
                                 any->path
                                 (nio/buffered-reader)
                                 (io/reader)
                                 (java.io.PushbackReader.))]
            (let [cfg-ovr (edn/read input)]
              (if (sdu/valid-overrides? cfg-ovr)
                cfg-ovr
                (throw
                 (ex-info
                  "Invalid overrides"
                  {:explanation (s/explain-data
                                 :spacetools.spacedoc.config/overriding-configs
                                 cfg-ovr)}))))))
        (fn [^Exception err]
          (exc/failure (ex-info "Can't read configuration overrides file"
                                {:path path :error err}))))))


(defn-spec *spit (exception-of? any?)
  "Like `spit` but also creates parent directories.
  Output is wrapped in try monad."
  [path file-ref? content any?]
  (io! (exc/try-or-recover
        (let [p (any->path path)
              a-path (absolute p)
              parent-dir (nio/parent a-path)]
          (nio/create-dirs parent-dir)
          (with-open [output (->> p (nio/buffered-writer) (io/writer))]
            (.write output (str content)))
          a-path)
        (fn [^Exception err]
          (exc/failure
           (ex-info "Can't write file" {:path path}))))))


(defn-spec *sdn-fps-in-dir (exception-of? set?)
  [path file-ref?]
  (io! (exc/try-on
        (let [p (any->path path)]
          (if (directory? p)
            (into #{}
                  (comp
                   (map (partial absolute))
                   (filter sdn-file?)
                   (map str))
                  (file-seq (nio/file p)))
            (throw
             ((cond
                (not (nio/exists? p)) (partial ex-info "File doesn't exists")
                (nio/file? p) (partial ex-info "File isn't a directory")
                (nio/readable? p) (partial ex-info "Directory isn't readable"))
              {:file p})))))))

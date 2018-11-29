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


(defn-spec *fp->sdn exc/exception?
  "Read and validate .SDN file."
  ([path ::file-path]
   (*fp->sdn :spacetools.spacedoc.node/root path))
  ([root-node-spec s/spec? path ::file-path]
   (io!
    (exc/try-or-recover
     (with-open [input (->> path
                            (io/file)
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


(defn-spec println-err nat-int?
  [& msg (s/* string?)]
  (io!
   (binding [*out* *err*]
     (println (str/join msg))
     2)))


(defn-spec println-ok nat-int?
  [& msg (s/* string?)]
  (io!
   (println (str/join msg))
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
  [*output exc/exception?]
  (io!
   (System/exit
    (let [output (m/extract *output)]
      (if (exc/failure? *output)
        (println-err
         (format "Error:\n%s\nRun with \"-h\" for usage" (err->msg output)))
        (println-ok output))))))


(defn-spec *slurp-cfg-overrides exc/exception?
  [overrides-fp ::file-path]
  (io!
   (exc/try-or-recover
    (when (edn-file? overrides-fp)
      (let [cfg-ovr (-> overrides-fp slurp edn/read-string)]
        (if (sdu/valid-overrides? cfg-ovr)
          cfg-ovr
          (throw
           (ex-info
            "Invalid overrides"
            {:explanation (s/explain-data
                           :spacetools.spacedoc.config/overriding-configs
                           cfg-ovr)})))))
    (fn [^Exception err]
      (exc/failure
       (ex-info "Can't read configuration overrides file"
                {:path overrides-fp :error err}))))))


(defn-spec *spit exc/exception?
  "Like `spit` but also creates parent directories.
  Output is wrapped in try monad."
  [path ::file-path content any?]
  (io!
   (exc/try-or-recover
    (let [a-path (absolute path)
          parent-dir (.getParent (io/file a-path))]
      (.mkdirs (io/file parent-dir))
      (spit path content)
      a-path)
    (fn [^Exception err]
      (exc/failure
       (ex-info "Can't write file" {:path path}))))))


(defn-spec *sdn-fps-in-dir exc/exception?
  [input-dir-path ::file-path]
  (io!
   (exc/try-on
    (if-not (directory? input-dir-path)
      (exc/failure (ex-info "File isn't a directory"
                            {:file input-dir-path}))
      (into #{}
            (comp
             (map #(.getCanonicalPath (io/file %)))
             (map str)
             (filter sdn-file?))
            (file-seq (io/file input-dir-path)))))))

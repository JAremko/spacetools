(ns spacetools.spacedoc-io.core
  "File-system I/O. Functions that can fail return `cats.monad.exception`."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [nio2.core :as nio]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.interface :as sdu])
  (:import (java.io File)
           (java.nio.file Path)))


(def filesystem (nio/default-fs))


(defn-spec path? boolean?
  "Returns true if X is a `Path` instance."
  [x any?]
  (instance? Path x))


(defn-spec file-or-dir? boolean?
  "Returns true if X is a `File` instance."
  [x any?]
  (instance? File x))


(s/def ::file-ref (s/or :string-path (s/and string? (complement str/blank?))
                        :path path?
                        :file-or-dir file-or-dir?))

(defn-spec file-ref? boolean?
  "Returns true if X is one of file reference types or a string."
  [x any?]
  (s/valid? ::file-ref x))


(defn-spec str->path path?
  "Construct `Path` form a `String`."
  [s string?]
  (nio/path filesystem s))


(defn-spec file-ref->path path?
  "Construct `Path` from a file reference."
  [f-ref file-ref?]
  ((condp #(%1 %2) f-ref
     string? str->path
     file-or-dir? (comp str->path str)
     path? identity
     (throw (ex-info "Argument isn't a file reference."
                     {:arg f-ref :arg-type (type f-ref)})))
   f-ref))


(defn-spec directory? boolean?
  "Returns true if X is a directory."
  [x any?]
  (io! (and (file-ref? x)
            (some-> x (file-ref->path) (nio/dir?)))))


(defn-spec file? boolean?
  "Returns true if X is a file but not a directory."
  [x any?]
  (io! (and (file-ref? x)
            (some-> x (file-ref->path) (nio/file?)))))


(defn-spec error? boolean?
  "Returns true if X is a `Throwable` instance."
  [x any?]
  (instance? Throwable x))


(defn-spec regexp? boolean?
  "Returns true if X is a regular expression pattern."
  [x any?]
  (instance? java.util.regex.Pattern x))


(s/def ::ext (s/and string? #(str/starts-with? % ".")))

(defn-spec file-with-ext? boolean?
  "Returns true if X is a `::file-ref` with extension EXT.
NOTE: EXT must include .(dot)"
  [ext ::ext x any?]
  (true?
   (when (file? x)
     (let [fp (file-ref->path x)]
       (some-> fp (str) (str/ends-with? ext))))))


(defn-spec sdn-file? boolean?
  "Returns true if X is a `::file-ref` with .sdn extension."
  [x any?]
  (file-with-ext? ".sdn" x))


(defn-spec edn-file? boolean?
  "Returns true if X is a `::file-ref` with .edn expression."
  [x any?]
  (file-with-ext? ".edn" x))


(defn-spec absolute path?
  "Return absolute version of the PATH."
  [path file-ref?]
  (io! (-> path (file-ref->path) (nio/absolute))))


(defn-spec rebase-path path?
  "Rebase PATH from OLD-BASE to NEW-BASE."
  [old-base file-ref? new-base file-ref? path file-ref?]
  (io! (let [[a-ob a-nb a-p] (map (comp str absolute file-ref->path)
                                  [old-base new-base path])]
         (str->path (str/replace-first a-p a-ob a-nb)))))


(defmacro exception-of?
  "Construct predicate function for testing exception monad value.
  The predicate returns true if the monad contains `exc/failure`
  or if `exc/success` wraps value satisfying PRED predicate.
  PRED also can be a spec or qualified-ident referencing a spec."
  [pred]
  `(fn [*val#]
     (and (exc/exception? *val#)
          (if (exc/success? *val#)
            (m/bind *val# #(if (or (qualified-ident? ~pred)
                                   (s/spec? ~pred))
                             (s/valid? ~pred %)
                             (~pred %)))
            true))))


(defn-spec *fp->sdn (exception-of? map?)
  "Read and validate .SDN file."
  ([path file-ref?]
   (*fp->sdn :spacetools.spacedoc.node/root path))
  ([root-node-spec (s/or :spec s/spec? :spec-ref qualified-ident?)
    path file-ref?]
   (io! (exc/try-or-recover
         (with-open [input (->> path
                                file-ref->path
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
            (ex-info (.getMessage err) (merge {:file path} (ex-data err)))))))))


(defn-spec output-err nat-int?
  "Print out MSG es into stderr and return 2."
  [& msg (s/* string?)]
  (io! (binding [*out* *err*]
         (println (str/join msg))
         2)))


(defn-spec output-ok nat-int?
  "Print out MSG es into stdout and return 0."
  [& msg (s/* string?)]
  (io! (println (str/join msg))
       0))


(defn-spec err->msg string?
  "Format error message ERR."
  [^Throwable err error?]
  (let [{:keys [cause data]} (Throwable->map err)]
    (str/join \newline
              (interleave
               (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                    ["Cause" "File" "Data"])
               [cause
                (or (:file data) "<none>")
                (or (:problems data) (seq data) "<none>")]))))


(defn exit
  "Call `System/exit` with status-code.
  NOTE: Useful for mocking."
  [status-code]
  (io! ;; :)
   (System/exit status-code)))


(defn-spec try-m->output nil?
  "Print *OUTPUT value to stderr or stdout and `System/exit` with code 0 or 2."
  [*output (exception-of? any?)]
  (io! (exit
        (let [output (m/extract *output)]
          (if (exc/failure? *output)
            (output-err
             (format "Error:\n%s\nRun with \"-h\" for usage" (err->msg output)))
            (output-ok (str output)))))))


(defn-spec *read-cfg-overrides (exception-of? map?)
  "Read and validate configuration overrides from a PATH file."
  [path file-ref?]
  (io! (exc/try-or-recover
        (if (edn-file? path)
          (with-open [input (->> path
                                 file-ref->path
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
                                 cfg-ovr)})))))
          (ex-info "Not an edn-file" {:path (str path)}))
        (fn [^Exception err]
          (exc/failure (ex-info "Can't read configuration overrides file"
                                {:path path :error err}))))))


(defn-spec *spit (exception-of? any?)
  "Spit CONTENT into PATH file and returns path wrapped into `exc/exception`."
  [path file-ref? content any?]
  (io! (exc/try-or-recover
        (let [p (file-ref->path path)
              a-path (absolute p)
              parent-dir (nio/parent a-path)]
          (nio/create-dirs parent-dir)
          (with-open [output (->> p (nio/buffered-writer) (io/writer))]
            (.write ^java.io.Writer output (str content)))
          ;;        ^^^^ Getting reflection warning for some reason ¯\_(ツ)_/¯
          a-path)
        (fn [^Exception err]
          (exc/failure
           (ex-info "Can't write file" {:path path}))))))


(defn-spec *slurp (exception-of? (s/coll-of string?))
  "Read content of the PATH file into array of strings (lines)."
  [path file-ref?]
  (io! (exc/try-or-recover
        (if (file? path)
          (->> path
               file-ref->path
               nio/read-all-lines
               vec)
          (ex-info "Isn't a file" {:path (str path)}))
        (fn [^Exception err]
          (exc/failure (ex-info "Can't read file" {:path path :error err}))))))


(defn-spec *fps-in-dir (exception-of?
                        (s/coll-of (s/and string?
                                          (complement str/blank?))
                                   :kind set?
                                   :min-count 0
                                   :distinct true))
  "Return absolute paths of files with EXT extension in PATH directory.
NOTE: EXT must include dot."
  [ext ::ext path file-ref?]
  (io! (exc/try-on
        (let [p (file-ref->path path)]
          (if (directory? p)
            (into #{}
                  (comp (map absolute)
                        (filter (partial file-with-ext? ext))
                        (map str))
                  (tree-seq nio/dir?
                            #(-> % nio/dir-stream (.iterator) (iterator-seq))
                            p))
            (throw
             ((cond
                (not (nio/exists? p)) (partial ex-info "File doesn't exists")
                (nio/file? p) (partial ex-info "File isn't a directory")
                (nio/readable? p) (partial ex-info "Directory isn't readable"))
              {:extension ext :file p})))))))


(defn-spec *flatten-fps (exception-of?
                         (s/coll-of (s/and string?
                                           (complement str/blank?))
                                    :kind set?
                                    :min-count 0
                                    :distinct true))
  "Flatten sequence of EXT file PATHS and directories(searched for files).
  EXT is a file extension (including dot)."
  [ext ::ext paths (s/coll-of file-ref? :min-count 0)]
  (exc/try-on
   (r/fold
    (r/monoid (m/lift-m 2 union) (exc/wrap hash-set))
    (r/map
     #(cond
        (file-with-ext? ext %) (exc/success (hash-set %))
        (directory? %) (*fps-in-dir ext %)
        :else (exc/failure
               (ex-info (str "File doesn't have proper extension"
                             " or isn't a readable directory.")
                        {:expected-extension ext :file %})))
     paths))))

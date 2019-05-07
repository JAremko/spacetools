(ns spacetools.fs-io.core
  "File-system I/O."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [nio2.core :as nio]
            [orchestra.core :refer [defn-spec]])
  (:import (java.io File)
           (java.nio.file Path)))


(def filesystem (nio/default-fs))


(defn-spec path? boolean?
  "Returns true if X is a `Path` instance."
  [x any?]
  (instance? Path x))


(s/def ::file-ref (s/or :string-path (s/and string? (complement str/blank?))
                        :path path?))


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
     path? identity
     (throw (ex-info "Argument isn't a file reference."
                     {:arg f-ref :arg-type (type f-ref)})))
   f-ref))


(defn-spec directory? boolean?
  "Returns true if X is a directory."
  [x any?]
  (and (file-ref? x)
       (some-> x (file-ref->path) (nio/dir?))))


(defn-spec file? boolean?
  "Returns true if X is a file but not a directory."
  [x any?]
  (and (file-ref? x)
       (some-> x (file-ref->path) (nio/file?))))


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
  (-> path (file-ref->path) (nio/absolute)))


(defn-spec rebase-path path?
  "Rebase PATH from OLD-BASE to NEW-BASE."
  [old-base file-ref? new-base file-ref? path file-ref?]
  (let [[a-ob a-nb a-p] (map (comp str absolute file-ref->path)
                             [old-base new-base path])]
    (str->path (str/replace-first a-p a-ob a-nb))))


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


(defn-spec output-err nat-int?
  "Print out MSG es into stderr and return 2."
  [& msg (s/* string?)]
  (binding [*out* *err*]
    (println (str/join msg))
    2))


(defn-spec output-ok nat-int?
  "Print out MSG es into stdout and return 0."
  [& msg (s/* string?)]
  (println (str/join msg))
  0)


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
  (System/exit status-code))


(defn-spec try-m->output nil?
  "Print *OUTPUT value to stderr or stdout and `System/exit` with code 0 or 2."
  [*output (exception-of? any?)]
  (exit
   (let [output (m/extract *output)]
     (if (exc/failure? *output)
       (output-err
        (format "Error:\n%s\nRun with \"-h\" for usage" (err->msg output)))
       (output-ok (str output))))))


(defn-spec *spit (exception-of? any?)
  "Spit CONTENT into PATH file and returns path wrapped into `exc/exception`."
  [path file-ref? content any?]
  (exc/try-or-recover
   (let [p (file-ref->path path)
         a-path (absolute p)
         parent-dir (nio/parent a-path)]
     (nio/create-dirs parent-dir)
     (with-open [output (->> p (nio/buffered-writer) (io/writer))]
       (.write output (str content)))
     a-path)
   (fn [^Exception err]
     (exc/failure
      (ex-info "Can't write file" {:path path})))))


(defn-spec *slurp (exception-of? (s/coll-of string?))
  "Read content of the PATH file into array of strings (lines)."
  [path file-ref?]
  (exc/try-or-recover
   (if (file? path)
     (->> path
          file-ref->path
          nio/read-all-lines
          vec)
     (ex-info "Isn't a file" {:path (str path)}))
   (fn [^Exception err]
     (exc/failure (ex-info "Can't read file" {:path path :error err})))))


(defn-spec *fps-in-dir (exception-of?
                        (s/coll-of (s/and string?
                                          (complement str/blank?))
                                   :kind set?
                                   :min-count 0
                                   :distinct true))
  "Return absolute paths of files with EXT extension in PATH directory.
NOTE: EXT must include dot."
  [ext ::ext path file-ref?]
  (exc/try-on
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
         {:extension ext :file p}))))))


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


(defn-spec normalize path?
  "Normalize PATH"
  [path file-ref?]
  (nio/normalize (file-ref->path path)))


(defn-spec join path?
  "Join PATH and PARENT path."
  [parent file-ref? path file-ref?]
  (nio/join (file-ref->path parent) (file-ref->path path)))


(defn-spec relativize path?
  "Relativize PATH relative to OTHER"
  [path file-ref? other file-ref?]
  (nio/relativize (file-ref->path path) (file-ref->path other)))

(defn-spec parent path?
  "Return parent dir of the PATH"
  [path file-ref?]
  (->> path file-ref->path nio/absolute nio/parent))

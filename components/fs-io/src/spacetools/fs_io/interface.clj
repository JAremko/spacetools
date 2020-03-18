(ns spacetools.fs-io.interface
  (:require [spacetools.fs-io.core :as io]
            [orchestra.core :refer [defn-spec]]))

(def filesystem io/filesystem)

(defn-spec absolute io/path?
  "Return absolute version of the PATH."
  [path io/file-ref?]
  (io/absolute path))

(defn-spec rebase-path io/path?
  "Rebase PATH from OLD-BASE to NEW-BASE."
  [old-base io/file-ref? new-base io/file-ref? path io/file-ref?]
  (io/rebase-path old-base new-base path))

(defn-spec *spit (io/exception-of? io/path?)
  "Spit CONTENT into PATH file and returns path wrapped into `exc/exception`."
  [path io/file-ref? content any?]
  (io/*spit path content))

(defn-spec *slurp (io/exception-of?  :spacetools.fs-io.core/strings)
  "Read content of the PATH file into array of strings (lines)."
  [path io/file-ref?]
  (io/*slurp path))

(defn-spec file? boolean?
  "Returns true if X is a file but not a directory."
  [x any?]
  (io/file? x))

(defn-spec file-ref? boolean?
  "Returns true if X is one of file reference types or a string."
  [x any?]
  (io/file-ref? x))

(defn-spec file-ref->path io/path?
  "Construct `Path` from a file reference."
  [f-ref file-ref?]
  (io/file-ref->path f-ref))

(defn-spec sdn-file? boolean?
  "Returns true if X is a `::file-ref` with .sdn extension."
  [x any?]
  (io/sdn-file? x))

(defn-spec edn-file? boolean?
  "Returns true if X is a `::file-ref` with .edn expression."
  [x any?]
  (io/edn-file? x))

(defn-spec directory? boolean?
  "Returns true if X is a directory."
  [x any?]
  (io/directory? x))

(defn-spec try-m->output any?
  "Print *OUTPUT value to stderr or stdout and `System/exit` with code 0 or 2."
  [*output (io/exception-of? any?)]
  (io/try-m->output *output))

(defn-spec *flatten-fps (io/exception-of?
                         :spacetools.fs-io.core/set-of-strings+)
  "Flatten sequence of EXT file PATHS and directories(searched for files).
  EXT is a file extension (including dot)."
  [ext :spacetools.fs-io.core/extension paths :spacetools.fs-io.core/file-refs]
  (io/*flatten-fps ext paths))

(defn-spec normalize io/path?
  "Normalize PATH"
  [path io/file-ref?]
  (io/normalize path))

(defn-spec join io/path?
  "Join PATH and PARENT path."
  [parent io/file-ref? path io/file-ref?]
  (io/join parent path))

(defn-spec relativize io/path?
  "Relativize PATH relative to OTHER"
  [path io/file-ref? other io/file-ref?]
  (io/relativize path other))

(defn-spec parent io/path?
  "Return parent dir of the PATH"
  [path io/file-ref?]
  (io/parent path))

(defmacro exception-of?
  "Construct predicate function for testing exception monad value.
  The predicate returns true if the monad contains `exc/failure`
  or if `exc/success` wraps value satisfying PRED predicate.
  PRED also can be a spec or qualified-ident referencing a spec."
  [pred]
  `(io/exception-of? ~pred))

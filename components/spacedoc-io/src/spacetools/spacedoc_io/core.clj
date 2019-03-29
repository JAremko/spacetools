(ns spacetools.spacedoc-io.core
  "File-system I/O. Functions that can fail return `cats.monad.exception`."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [nio2.core :as nio]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface
             :refer [filesystem exception-of? file-ref?] :as fio]
            [spacetools.spacedoc.interface :as sdu]))


(defn-spec *fp->sdn (exception-of? map?)
  "Read and validate .SDN file."
  ([path file-ref?]
   (*fp->sdn :spacetools.spacedoc.node/root path))
  ([root-node-spec (s/or :spec s/spec? :spec-ref qualified-ident?)
    path file-ref?]
   (io! (exc/try-or-recover
         (with-open [input (->> path
                                fio/file-ref->path
                                nio/buffered-reader
                                io/reader
                                java.io.PushbackReader.)]
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


(defn-spec *read-cfg-overrides (exception-of? map?)
  "Read and validate configuration overrides from a PATH file."
  [path file-ref?]
  (io! (exc/try-or-recover
        (if (fio/edn-file? path)
          (with-open [input (->> path
                                 fio/file-ref->path
                                 nio/buffered-reader
                                 io/reader
                                 java.io.PushbackReader.)]
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

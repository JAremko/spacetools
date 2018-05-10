(ns spacedoc.actions
  (:require [spacedoc.io :as sio]
            #_ [spacedoc.viz :as viz]
            [spacedoc.util :as util]
            [clojure.string :refer [join]]
            [spacedoc.data :as data]
            [cats.core :as m]
            [clojure.edn :as edn]
            [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]))


(defn validate
  "Validate Spacedoc files with specs."
  [sdn-file-paths]
  (exc/try-on
   (m/alet [spacedocs (m/sequence (pmap sio/fp->spacedoc sdn-file-paths))]
           (exc/success (format "%s spacedoc files successfully validated."
                                (count spacedocs))))))


(defn describe-spec
  "Describe spec by qualified keyword."
  [spec-key]
  (exc/try-on
   (let [key (edn/read-string spec-key)]
     (if (qualified-keyword? key)
       (exc/success (s/describe key))
       (exc/failure (ex-info "Spec key must be a qualified keyword"
                             {:keyword key}))))))


(defn relations
  "Output nodes relations in SDN files."
  [sdn-file-paths]
  (exc/try-on
   (m/alet [spacedocs (m/sequence (pmap
                                   (partial sio/fp->spacedoc :spacedoc.data/any)
                                   sdn-file-paths))]
           (join \newline (data/node-relations-aggregate spacedocs)))))


(defn draw-relations-graph
  "Draw SVG of nodes relations."
  [svg-file-path sdn-file-paths]
  (exc/try-on
   (exc/failure
    (ex-info "This action is disabled for GraalVM compatibility." {}))
   #_ (m/alet [spacedocs (m/sequence (pmap
                                      (partial sio/fp->spacedoc :spacedoc.data/any)
                                      sdn-file-paths))]
              (cond
                (sio/directory? svg-file-path)
                (exc/failure (ex-info "Output file path is a directory."
                                      {:output-file-path svg-file-path}))
                (sio/parent-dir-writable? svg-file-path)
                (sio/export-graph-svg
                 svg-file-path
                 (viz/build-graph (data/node-relations-aggregate spacedocs)))
                :else
                (exc/failure (ex-info "Parent directory isn't writable."
                                      {:output-file-path svg-file-path}))))))

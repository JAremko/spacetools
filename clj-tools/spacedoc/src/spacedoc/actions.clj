(ns spacedoc.actions
  (:require [spacedoc.io :as sio]
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
           (format "%s spacedoc files successfully validated."
                   (count spacedocs)))))


(defn describe-spec
  "Describe spec by qualified keyword."
  [spec-key]
  (exc/try-on
   (let [key (edn/read-string spec-key)]
     (if (qualified-keyword? key)
       (s/describe key)
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

(ns spacedoc.actions
  (:require [spacedoc.io :as sio]
            [spacedoc.data :as data]
            [spec-tools.parse :as sp]
;;            [spacedoc.data.org :refer [sdn->org]]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.string :refer [join]]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))


(defn validate
  "Validate Spacedoc files with specs."
  [sdn-file-paths]
  (exc/try-on
   (m/alet [spacedocs (m/sequence (pmap sio/fp->spacedoc sdn-file-paths))]
           (format "%s spacedoc files successfully validated."
                   (count spacedocs)))))


;; (defn orgify
;;   "Export Spacedoc files into \"target/orgify/\" directory as .org files."
;;   [sdn-file-paths]
;;   (exc/try-on
;;    (m/alet [spacedocs (m/sequence (pmap sio/fp->spacedoc sdn-file-paths))]
;;            (format "%s spacedoc files successfully validated."
;;                    (count spacedocs)))))


(defn describe-spec
  "Describe spec by qualified keyword."
  [spec-key]
  (exc/try-on
   (let [key (edn/read-string spec-key)]
     (if (qualified-keyword? key)
       (let [{:keys [type keys]}(sp/parse-spec key)]
         (str "Type: " (name type)
              \newline
              "Keys: " keys
              \newline
              "Spec: " (s/form key)
              \newline))
       (exc/failure (ex-info "Spec key must be a qualified keyword"
                             {:keyword key}))))))


(defn relations
  "Output nodes relations in SDN files."
  [sdn-file-paths]
  (exc/try-on
   (m/alet [spacedocs (m/sequence (pmap
                                   (partial sio/fp->spacedoc :spacedoc.data/any)
                                   sdn-file-paths))]
           (join \newline (data/node-relations-aggregate (vec spacedocs))))))

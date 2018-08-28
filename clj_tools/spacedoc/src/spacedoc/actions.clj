(ns spacedoc.actions
  (:require [spacedoc.io :as sio]
            [spacedoc.data :as data]
            [spacedoc.args :refer [*parse *parse-fs]]
            [spacedoc.data.org :refer [sdn->org]]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))


(:tag (s/get-spec :spacedoc.data/root))

(defn *validate
  "Validate Spacedoc files with specs."
  [fs]
  (exc/try-on
   (m/mlet [sdn-fps (*parse-fs fs)
            spacedocs (m/sequence (pmap sio/*fp->spacedoc sdn-fps))]
           (format "%s .sdn files have been successfully validated."
                   (count spacedocs)))))


(defn *orgify
  "Export .SDN files to TARGET-DIR as .ORG files."
  [src-dir target-dir fs]
  (exc/try-on
   (m/mlet [sdn-fps (*parse-fs fs)
            spacedocs (m/sequence (pmap sio/*fp->spacedoc sdn-fps))
            orgs (m/sequence
                  (pmap
                   (fn [path cont]
                     (let [new-path
                           (str/replace
                            (sio/rebase-path src-dir target-dir path)
                            #"(?ix)\.sdn$" ".org")]
                       (sio/*spit new-path (sdn->org cont))))
                   sdn-fps
                   spacedocs))]
           (format (str "%s .sdn files have been successfully exported "
                        "to \"%s\" directory as .org files")
                   (count orgs)
                   (sio/absolute target-dir)))))


(defn *describe-spec
  "Describe spec by qualified keyword."
  [spec-key]
  (exc/try-on
   (let [key (edn/read-string spec-key)]
     (if (qualified-keyword? key)
       (s/describe* (s/get-spec key))
       (exc/failure (ex-info "Spec key must be a qualified keyword"
                             {:keyword key}))))))


(defn *relations
  "Output nodes relations in SDN files."
  [fs]
  (exc/try-on
   (m/mlet [sdn-fps (*parse-fs fs)
            spacedocs (m/sequence (pmap
                                   (partial sio/*fp->spacedoc :spacedoc.data/any)
                                   sdn-fps))]
           (str
            "[<NODE_TAG> <FOUND_CHILDREN_TAGS>]\n"
            (str/join \newline
                      (data/node-relations-aggregate (vec spacedocs)))))))

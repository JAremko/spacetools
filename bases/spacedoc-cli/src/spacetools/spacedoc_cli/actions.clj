(ns spacetools.spacedoc-cli.actions
  "Actions that the app can perform."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacetools.spacedoc-cli.args :refer [*parse-input-files]]
            [spacetools.spacedoc-io.interface :as sio]
            [spacetools.spacedoc.interface :as sd]))


(defn *validate
  "Validate Spacemacs documentation files with specs."
  [fs]
  (m/mlet
   [sdn-fps (*parse-input-files fs)
    docs (m/sequence (pmap sio/*fp->sdn sdn-fps))]
   (m/return (format "%s Documentation files have been successfully validated."
                     (count docs)))))


(defn *orgify
  "Export .SDN files to TARGET-DIR as .ORG files."
  [src-dir target-dir fs]
  (letfn [(org-path [edn-path]
            (str/replace (sio/rebase-path src-dir target-dir edn-path)
                         #"(?ix)\.sdn$" ".org"))
          (export-to-org [fp content]
            (sio/*spit (org-path fp)
                       (->> content (sd/up-tags src-dir fp) (sd/sdn->org))))]
    (m/mlet [sdn-fps (*parse-input-files fs)
             docs (m/sequence (pmap sio/*fp->sdn sdn-fps))
             orgs (m/sequence (pmap export-to-org sdn-fps docs))]
            (m/return (format (str "%s .sdn files have been successfully "
                                   "exported to \"%s\" directory as .org files")
                              (count orgs)
                              (sio/absolute target-dir))))))


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
  (m/mlet [sdn-fps (*parse-input-files fs)
           docs (->> sdn-fps
                     (pmap (partial sio/*fp->sdn :spacetools.spacedoc.node/any))
                     (m/sequence))]
          (m/return (str "[<NODE_TAG> <FOUND_CHILDREN_TAGS>]\n"
                         (str/join \newline (sd/relations (vec docs)))))))

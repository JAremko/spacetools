(ns spacetools.spacedoc-cli.actions
  "Actions that the app can perform."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacetools.spacedoc-cli.args :refer [*parse-fs]]
            [spacetools.spacedoc.interface :as sd]
            [spacetools.spacedoc-cli.io :as sio]))


(defn *validate
  "Validate Spacetools.Spacedoc-Cli files with specs."
  [fs]
  (exc/try-on
   (m/mlet [sdn-fps (*parse-fs fs)
            docs (m/sequence (pmap sio/*fp->sdn sdn-fps))]
           (format "%s Documentation files have been successfully validated."
                   (count docs)))))


(defn *orgify
  "Export .SDN files to TARGET-DIR as .ORG files."
  [src-dir target-dir fs]
  (exc/try-on
   (m/mlet [sdn-fps (*parse-fs fs)
            docs (m/sequence (pmap sio/*fp->sdn sdn-fps))
            orgs (m/sequence
                  (pmap
                   (fn [path cont]
                     (let [new-path
                           (str/replace
                            (sio/rebase-path src-dir target-dir path)
                            #"(?ix)\.sdn$" ".org")]
                       (sio/*spit new-path
                                  (->> cont
                                       (sd/up-tags src-dir path)
                                       (sd/sdn->org)))))
                   sdn-fps
                   docs))]
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
            docs (->> sdn-fps
                      (pmap (partial sio/*fp->sdn
                                     :spacetools.spacedoc.node/any))
                      (m/sequence))]
           (str
            "[<NODE_TAG> <FOUND_CHILDREN_TAGS>]\n"
            (str/join \newline
                      (sd/rels-aggr (vec docs)))))))

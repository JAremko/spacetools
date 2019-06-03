(ns spacetools.spacedoc-cli.actions
  "Actions that the app can perform."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface :as io :refer [exception-of?]]
            [spacetools.spacedoc-cli.args :refer [*parse-input-files]]
            [spacetools.spacedoc-io.interface :as sio]
            [spacetools.spacedoc.interface :as sd]))


(defn-spec *validate (exception-of? string?)
  "Validate Spacemacs documentation files with specs."
  [fs (s/coll-of (s/and string? (some-fn io/directory? io/sdn-file?))
                 :min-count 1)]
  (m/mlet
   [sdn-fps (*parse-input-files fs)
    docs (m/sequence (pmap sio/*fp->sdn sdn-fps))]
   (m/return (format "%s Documentation files have been successfully validated."
                     (count docs)))))


(defn-spec *orgify (exception-of? string?)
  "Export .SDN files from SRC-DIR to TARGET-DIR as .ORG files."
  [src-dir (s/and string? io/directory?) target-dir string?]
  (m/do-let [sdn-fps (*parse-input-files [src-dir])
             docs (m/sequence (pmap sio/*fp->sdn sdn-fps))
             orgs (m/sequence (pmap export-to-org sdn-fps docs))]
            (io/*spit (->> sdn-fps
                           (io/rebase-path src-dir target-dir)
                           (sio/set-ext ".org"))
                      (sd/sdn->org docs))
            (m/return (format (str "%s .sdn files have been successfully "
                                   "exported to \"%s\" directory as .org files")
                              (count orgs)
                              (io/absolute target-dir)))))


(defn-spec *describe-spec (exception-of? string?)
  "Describe spec by qualified keyword."
  [spec-key string?]
  (exc/try-on
   (let [key (edn/read-string spec-key)]
     (if (qualified-keyword? key)
       (if-let [spc (s/get-spec key)]
         (str (s/describe* spc))
         (exc/failure (ex-info "Spec is undefined"
                               {:keyword key})))
       (exc/failure (ex-info "Spec key must be a qualified keyword"
                             {:keyword key}))))))


(defn-spec *layers (exception-of? string?)
  "Create LAYERS.sdn file in DIR using SDN files from the directory."
  [dir string?]
  (m/do-let
   [sdn-fps (*parse-input-files dir)
    docs (m/sequence (pmap sio/*fp->sdn sdn-fps))]
   (->> docs
        (map (fn fix-paths
               [fp doc]
               (as-> doc $
                 (sio/re-root-sdn dir fp $)
                 (update $ :source #(->> % (sio/set-ext ".org") str))
                 (sio/re-root-relative-links dir fp $)))
             sdn-fps)
        (sd/layers-sdn)
        (io/*spit (io/join dir "LAYERS.sdn")))
   (m/return (format (str "%s Documentation files processed."
                          " LAYERS.org created in \"%s\" directory.")
                     (count docs)
                     dir))))


(defn-spec *relations (exception-of? string?)
  "Output nodes relations in SDN files."
  [fs (s/coll-of (s/and string? (some-fn io/directory? io/sdn-file?))
                 :min-count 1)]
  (m/mlet [sdn-fps (*parse-input-files fs)
           docs (->> sdn-fps
                     (pmap (partial sio/*fp->sdn
                                    :spacetools.spacedoc.node/any-node))
                     (m/sequence))]
          (m/return (str "[<NODE_TAG> <FOUND_CHILDREN_TAGS>]\n"
                         (str/join \newline (sd/relations (vec docs)))))))

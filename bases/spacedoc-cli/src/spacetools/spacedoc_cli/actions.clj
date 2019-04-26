(ns spacetools.spacedoc-cli.actions
  "Actions that the app can perform."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface :refer [exception-of?]]
            [spacetools.fs-io.interface :as io]
            [spacetools.spacedoc-cli.args :refer [*parse-input-files]]
            [spacetools.spacedoc-io.interface :refer [*fp->sdn]]
            [spacetools.spacedoc.interface :as sd]))


(defn-spec *validate (io/exception-of? string?)
  "Validate Spacemacs documentation files with specs."
  [fs (s/coll-of (s/and string? (some-fn io/directory? io/sdn-file?))
                 :min-count 1)]
  (m/mlet
   [sdn-fps (*parse-input-files fs)
    docs (m/sequence (pmap *fp->sdn sdn-fps))]
   (m/return (format "%s Documentation files have been successfully validated."
                     (count docs)))))


(defn-spec *layers (io/exception-of? string?)
  "Create LAYERS.sdn file in DIR using SDN files from the directory."
  [dir string?]
  (m/do-let
   [sdn-fps (*parse-input-files dir)
    docs (m/sequence (pmap *fp->sdn sdn-fps))]
   (->> docs
        (map (fn [path doc]
               (assoc doc :source
                      (str "file:" (str/replace (io/relativize dir path)
                                                #"(?ix)\.sdn$" ".org"))))
             sdn-fps)
        sd/layers-sdn
        (io/*spit (io/join dir "LAYERS_WIP.sdn")))
   (m/return (format (str "%s Documentation files processed."
                          " LAYERS.org created in \"%s\" directory.")
                     (count docs)
                     dir))))


(defn-spec *orgify (io/exception-of? string?)
  "Export .SDN files from SRC-DIR to TARGET-DIR as .ORG files."
  [src-dir (s/and string? io/directory?) target-dir string?]
  (letfn [(org-path [edn-path]
            (str/replace (io/rebase-path src-dir target-dir edn-path)
                         #"(?ix)\.sdn$" ".org"))
          (export-to-org [fp content]
            (io/*spit (org-path fp)
                      (->> content (sd/up-tags src-dir fp) (sd/sdn->org))))]
    (m/mlet [sdn-fps (*parse-input-files [src-dir])
             docs (m/sequence (pmap *fp->sdn sdn-fps))
             orgs (m/sequence (pmap export-to-org sdn-fps docs))]
            (m/return (format (str "%s .sdn files have been successfully "
                                   "exported to \"%s\" directory as .org files")
                              (count orgs)
                              (io/absolute target-dir))))))


(defn-spec *describe-spec (io/exception-of? string?)
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


(defn-spec *relations (io/exception-of? string?)
  "Output nodes relations in SDN files."
  [fs (s/coll-of (s/and string? (some-fn io/directory? io/sdn-file?))
                 :min-count 1)]
  (m/mlet [sdn-fps (*parse-input-files fs)
           docs (->> sdn-fps
                     (pmap (partial *fp->sdn
                                    :spacetools.spacedoc.node/any-node))
                     (m/sequence))]
          (m/return (str "[<NODE_TAG> <FOUND_CHILDREN_TAGS>]\n"
                         (str/join \newline (sd/relations (vec docs)))))))

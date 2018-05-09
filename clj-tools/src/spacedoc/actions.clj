(ns spacedoc.actions
  (:require [spacedoc.io :as sio]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [clojure.spec.alpha :as s]))


(defn validate-input-exc
  "Validate "
  [sdn-file-paths]
  (exc/try-on
   (m/alet [out (m/sequence (map sio/fp->spacedoc-exc sdn-file-paths))]
           (format "%s spacedoc files successfully validated.\n"
                   (count out)))))


(defn describe-spec-exc
  [spec-key]
  (exc/try-on "described"))


(defn draw-relations-graph-exc
  [svg-file-path sdn-file-paths]
  (exc/try-on "drawn"))

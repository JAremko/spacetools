(ns spacedoc.args
  (:require [spacedoc.io :as sio]
            [spacedoc.util :as util]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [cats.monad.exception :as exc]
            [lacij.view.graphview :as gv]
            [clojure.tools.cli :refer [parse-opts]]
            [spacedoc.data :as data]
            [cats.core :as m]
            [clojure.core.reducers :as r]))


(defn- make-input-op
  []
  (let [it ["-i" "--input DIRECTORY" "Inpu directory."
            :default-desc "<SPACEDOC_ROOT>/emacs-tools/export/target/"
            :validate [sio/directory? "Input isn't a directory."]]
        d-dir-exc (sio/default-input-dir-exc)]
    (if (exc/success? d-dir-exc)
      (vec (concat it [:default (m/extract d-dir-exc)]))
      it)))


(defn make-ops
  []
  [(make-input-op)
   ["-r" "--relations FILE" "Draw SVG of node relations into FILE"]
   ["-d" "--describe SPEC" "Describe spec by keyword"]
   ["-h" "--help"]])


(defn args->options
  [args]
  (let [{{:keys [input relations describe help]} :options
         errors :errors} (parse-opts args (make-ops))]
    (if errors
      (sio/exit-err (str "Bad args: " errors))
      :options)))

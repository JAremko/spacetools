(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :refer [parse-exc parse-input-exc]]
            [spacedoc.actions :as ac]
            [spacedoc.util :refer [fail]]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [join]]
            [clojure.edn :as edn]
            [cats.core :as m]
            [cats.monad.exception :as exc])
  (:gen-class))


(defn usage [options-summary]
  (join
   \newline
   ["Spacemacs documentation tools."
    ""
    "Usage: spacedoc [OPTIONS]... [ACTION]"
    ""
    "Options:"
    options-summary
    ""
    "Actions:"
    "  validate           Validate Spacedoc(SDN) input files."
    "  describe  SPEC     Describe spec by keyword."
    "  rel-graph OUT_FILE Draw SVG of node relations in input Spacedoc(SDN) fs."
    ""]))


(def ops
  [["-i" "--input INPUT" "Input directory or file. Can be reused."
    :parse-fn #(edn/read-string (str "\"" % "\""))
    :validate [(fn [in]
                 (and (string? in) (or (sio/directory? in) (sio/sdn-file? in))))
               "Input should be a directory or a .SDN file."]
    :assoc-fn (fn [m key val] (update m key (partial concat (list val))))]
   ["-r" "--relations OUT_FILE" "Draw SVG of node relations in input SND files."
    :validate [sio/parent-dir-writable? "Parent directory isn't writable."]]
   ["-d" "--describe SPEC" "Describe spec by keyword."
    :parse-fn edn/read-string
    :validate [keyword? "Specs should be specified by keyword."
               qualified-keyword? "Spec should be qualified keyword."]]
   ["-v" "--validate" "Validate Spacedoc(SDN) input files while reading."]
   ["-h" "--help" "Show help message."]])


(defn -main [& args]
  (let
      [output
       (m/alet
        [{:keys [help input summary action a-args]} (parse-exc args ops)]
        (if help
          (exc/success (usage summary))
          (match
           ;; Handlers
           [action      a-args]
           ["describe"  [key]]
           (m/fmap ac/describe-spec-exc key)
           ["rel-graph" [path]]
           (m/fmap (partial ac/draw-relations-graph-exc path) (parse-input-exc input))
           ["validate"  []]
           (m/fmap ac/validate-input-exc (parse-input-exc input))
           ;; Errors
           ["describe"  _]
           (fail "Describe requires keyword as a single arg." {:args a-args})
           ["rel-graph" _]
           (fail "rel-graph requires file name as a single arg." {:args a-args})
           ["validate"  _]
           (fail "Validate doesn't take args." {:args a-args})
           :else (ex-info "Invalid action" {:action action}))))]
    (println output)))



;; spacedocs (m/fmap #(eduction sio/fp->spacedoc-exc %) (:files-exc options))]
;; (if (seq spacedocs)
;;   (printf "%s Spacedocs successfully parsed and validated.\n"
;;           (count spacedocs))
;;   (println "Input folder doesn't contain SDN files.")))))

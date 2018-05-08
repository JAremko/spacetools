(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :as a]
            [clojure.edn :as edn]
            [cats.core :as m])
  (:gen-class))


(def ops
  [["-i" "--input INPUT" "Input directory or file. Can be reused."
    :default-desc "<SPACEDOC_ROOT>/emacs-tools/export/target/"
    :parse-fn #(list (edn/read-string (str "\"" % "\"")))
    ;; :validate [(fn [[in]]
    ;;              (and (string? in) (or (sio/directory? in) (sio/sdn-file? in))))
    ;;            "Input should be a directory or a .SDN file."]
    :assoc-fn (fn [m key val] (update m key (partial concat val)))]
   ["-r" "--relations FILE" "Draw SVG of node relations into FILE."
    :validate [sio/parent-dir-writable? "Parent directory isn't writable."]]
   ["-d" "--describe SPEC" "Describe spec by keyword."
    :parse-fn edn/read-string
    :validate [keyword? "Specs should be specified by keyword."
               qualified-keyword? "Spec should be qualified keyword."]]
   ["-v" "--validate" "Validate Spacedoc(SDN) input files while reading."]
   ["-h" "--help" "Show help message."]])


(defn -main [& args]
  (println
   (m/mlet [{:keys [help input validate relations describe]} (a/parse args ops)]
           {:h help :i input :pi (a/parse-input input) :v validate :r relations :d describe})))
           #_ (cond->>)

           ;; spacedocs (m/fmap #(eduction sio/fp->spacedoc-exc %) (:files-exc options))]
           ;; (if (seq spacedocs)
           ;;   (printf "%s Spacedocs successfully parsed and validated.\n"
           ;;           (count spacedocs))
           ;;   (println "Input folder doesn't contain SDN files.")))))

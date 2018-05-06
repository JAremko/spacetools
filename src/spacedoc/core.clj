(ns spacedoc.core
  (:gen-class)
  (:require [spacedoc.io :as sio]
            [spacedoc.util :as util]))


(def ops
  [["-i" "--input DIRECTORY" "Inpu directory"
    :validate [sio/directory? "Input isn't a directory."]]
   ["-h" "--help"]])


(defn -main [& args]
  (when-let [c (->> args
                    (sio/args->spacedocs-exc ops)
                    (util/spacedocs-exc->spacedocs)
                    (count))]
    (println "Spacedocs successfully parsed and validated.")
    (println "Total count:" c)))

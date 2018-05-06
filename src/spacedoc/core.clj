(ns spacedoc.core
  (:require [spacedoc.io :as sio]
            [spacedoc.args :as ag]
            [cats.core :as m]
            [spacedoc.util :as util])
  (:gen-class))


(defn -main [& args]
  (let [options (ag/args->options args)
        s-exc (sio/input-dir->spacedocs-exc (:input options))
        spacedocs (util/spacedocs-exc->spacedocs s-exc)]
    (if (seq spacedocs)
      (printf "%s Spacedocs successfully parsed and validated.\n"
              (count spacedocs))
      (println "Input folder doesn't contain SDN files."))))

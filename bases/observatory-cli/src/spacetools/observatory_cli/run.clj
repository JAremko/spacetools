(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [spacetools.observatory-cli.elisp.core :as el]))

(defn -main [file & args]
  (let [files (->> file
                   clojure.java.io/file
                   file-seq
                   (keep (comp (partial re-matches #".*\.el$") str))
                   (map #(vector % (slurp %))))]
    (time (vec (map #(do (prn (first %))
                         (el/read-str (second %)))
                    files))))
  (prn "Done!"))

(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [spacetools.observatory-cli.parsel :as p]))

(defn -main [file & args]
  (let [files (->> file
                   clojure.java.io/file
                   file-seq
                   (keep (comp (partial re-matches #".*\.el$") str))
                   (map #(vector % (slurp %))))]
    (time (vec (map #(do (prn (first %))
                         (p/elisp-str->pruned-parse-tree (second %)))
                    files))))
  (prn "Done!"))

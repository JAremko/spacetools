(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [spacetools.observatory-cli.elisp.parser :as p]
            [spacetools.observatory-cli.elisp.ast :as a]))

(defn -main [file & args]
  (let [files (->> file
                   clojure.java.io/file
                   file-seq
                   (keep (comp (partial re-matches #".*\.el$") str))
                   (map #(vector % (slurp %))))]
    (time (vec (map #(do (prn (first %))
                         ((comp a/parse-tree->ast
                                p/elisp-str->pruned-parse-tree
                                second) %))
                    files))))
  (prn "Done!"))

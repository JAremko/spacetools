(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [instaparse.core :as insta]
            [spacetools.observatory-cli.parsel :as p]))

(defn -main [file & args]
  (let [files (->> file
                   clojure.java.io/file
                   file-seq
                   (keep (comp (partial re-matches #".*\.el$") str))
                   (map #(vector % (slurp %))))]
    (mapv #(let [parses-c (count (insta/parses p/elisp-parser (second %)))]
             (when (not= parses-c 1)
               (prn (first %) parses-c)))
          files)
    (time (mapv #(do (prn (first %))
                     (p/elisp-str->edn (second %)))
                files)))
  (prn "Done!"))

;; (def foo (->> "/mnt/workspace/spacemacs-pr"
;;               clojure.java.io/file
;;               file-seq
;;               (keep (comp (partial re-matches #".*\.el$") str))
;;               (map #(vector % (slurp %)))))


;; (mapv #(let [parses-c (count (insta/parses p/elisp-parser (second %)))]
;;          (when (not= parses-c 1)
;;            (prn (first %) parses-c)))
;;       foo)

;; (-main "/mnt/workspace/spacemacs-pr")

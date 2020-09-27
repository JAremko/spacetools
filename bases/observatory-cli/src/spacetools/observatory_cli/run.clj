(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [spacetools.observatory-cli.parsel :refer [elisp-str->edn]]))

(defn -main [file & args]
  (let [foo (slurp file)]
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))))

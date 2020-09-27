(ns spacetools.observatory-cli.run
  (:gen-class)
  (:require [instaparse.core :as insta]
            [spacetools.observatory-cli.parsel :as p :refer [elisp-str->edn]]))

(defn -main [file & args]
  (let [foo (slurp file)]
    (prn (count (insta/parses p/elisp-parser foo)))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))
    (time (elisp-str->edn foo))))

(ns spacedoc.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [spacedoc.viz :as viz]
            [spacedoc.data :as data]
            [spacedoc.io :as io]))


(def ops
  [["-i" "--input DIRECTORY" "Inpu directory"
    :validate [io/directory? "Input isn't a directory."]]
   ["-h" "--help"]])


;; NOTE: Doesn't work with lain run
(defn -main [& args]
  (let [{{:keys [input]} :options errors :errors} (parse-opts args ops)]
    #_ (println input errors)
    (println (io/root-dir))))


;; (def doc-dir (clojure.java.io/file "emacs-tools/export/target"))


;; (def edn-files (io/edn-files-in-dir doc-dir))


;; (def spacedocs (pmap io/fp->*spacedoc-m edn-files))


;; (println (ex-data (:e (first (filter exc/failure? spacedocs)))))
;; (println (:e (first (filter exc/failure? spacedocs))))


;; (io/export-graph-svg "graph.svg" (viz/build-graph (apply data/node-relations (map deref spacedocs))))

;; (println (viz/build-graph (apply data/node-relations (map deref spacedocs))))

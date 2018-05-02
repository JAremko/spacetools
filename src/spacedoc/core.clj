(ns spacedoc.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [lower-case]]
            [criterium.core :as c]
            [cats.monad.exception :as exc]
            [spacedoc.viz :as viz]
            [spacedoc.data :as data]
            [spacedoc.io :as io]))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def doc-dir (clojure.java.io/file "emacs-tools/export/target"))


(def edn-files (io/edn-files-in-dir doc-dir))


(def spacedocs (pmap io/fp->spacedoc edn-files))

#_ (println (first (filter exc/failure? spacedocs)))


(viz/draw-graph-svg "graph.svg" (data/node-relations-aggregate (map deref spacedocs)))


#_ (c/with-progress-reporting (c/bench (doall (data/node-graph-aggregate spacedocs)) :verbose))

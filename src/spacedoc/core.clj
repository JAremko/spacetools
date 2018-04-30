(ns spacedoc.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [lower-case]]
            [spacedoc.viz :as viz]
            [spacedoc.data :as data]
            [spacedoc.io :as io]
            [spacedoc.conf :as conf]))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def doc-dir (clojure.java.io/file "emacs-tools/export/target"))


(def edn-files (io/edn-files-in-dir doc-dir))


(def spacedocs (pmap io/fp->spacedoc edn-files))


(map println (data/node-graph spacedocs))

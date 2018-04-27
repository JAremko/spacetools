(ns spacedoc.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [lower-case]]
            [spacedoc.viz :as viz]
            [spacedoc.data :as data]
            [spacedoc.util :as util]
            [spacedoc.conf :as conf]))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def doc-dir (clojure.java.io/file "emacs-tools/export/target"))

(def spacedoc-file-paths
  (->> doc-dir
       (file-seq)
       (filter #(.isFile %))
       (map str)
       (filter #(.endsWith (lower-case %) ".edn"))
       (vec)))

(def spacedocs (pmap util/fp->spacedoc spacedoc-file-paths))

(map println (util/node-graph spacedocs))

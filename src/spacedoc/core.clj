(ns spacedoc.core
  (:gen-class)
  (:use [clojure.core.reducers :only [fold]]
        [clojure.set :as set :only [union]]
        [clojure.string :only [lower-case]])
  (:require [clojure.spec.alpha :as s]
            [spacedoc.data :as data]
            [spacedoc.util :as util]
            [spacedoc.conf :as conf]))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def doc-dir (clojure.java.io/file "resources/import"))

(def file-paths-all
  (->> doc-dir
       (file-seq)
       (filter #(.isFile %))
       (map str)
       (into #{})))

(def file-paths-edn
  (filter #(.endsWith (clojure.string/lower-case %) ".edn")
          file-paths-all))


(def edn-file-paths->edn (apply util/edn-fps->fp->edn file-paths-edn))

(def node-tree-seq
  (partial tree-seq :children :children))

(defn node-children
  [tag in-doc]
  (->> in-doc
       (node-tree-seq)
       (filter #(= (:tag %) tag))
       (map :children)
       (flatten)))

(defn node-children-tag-set
  [tag in-doc]
  (->> in-doc
       (node-children tag)
       (map :tag)
       (into #{})))

(defn node-children-tag-set-in-docs
  [tag docs]
  (apply set/union
         (map #(node-children-tag-set tag %)
              docs)))

(defn explain-all
  [spec docs-map]
  (doall (map #(when-not (s/valid? spec (val %))
                 (println "Spec failed in:" (key %))
                 (s/explain spec (val %)))
              docs-map)))

(println (node-children-tag-set-in-docs
          :headline
          (map val edn-file-paths->edn)))

(explain-all :spacedoc.data/root edn-file-paths->edn)

(println "====== OK =========")

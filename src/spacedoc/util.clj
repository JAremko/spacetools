(ns spacedoc.util
  (:gen-class)
  (:use [clojure.core.reducers :only [fold]]
        [clojure.set :as set :only [union]]
        [clojure.string :only [lower-case]])
  (:require [clojure.spec.alpha :as s]
            [spacedoc.data :as data]
            [spacedoc.conf :as conf]))


(defn edn-fps->fp->edn
  ([edn-file-path]
   (try {edn-file-path
         (read-string (slurp edn-file-path))}
        (catch Exception e
          (throw
           (Exception.
            (format
             "\"%s\" During parsing of \"%s\" file"
             e
             edn-file-path))))))
  ([edn-file-path & fs]
   (fold
    conf/*n-threads*
    (fn ([xs x] (merge xs x)) ([] {}))
    (fn [xs x] (conj xs (edn-fps->fp->edn x)))
    (conj fs edn-file-path))))


(defn foo [& files]
  (println (first files)))


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

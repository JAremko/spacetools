(ns spacedoc.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [spacedoc.viz :as viz]
            [spacedoc.data :as data]
            [spacedoc.io :as io]))


(def cli-options
  ;; An option with a required argument
  [["-p" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ;; A non-idempotent option
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])


(defn -main [& args]
  (parse-opts args cli-options))


(def doc-dir (clojure.java.io/file "emacs-tools/export/target"))


(def edn-files (io/edn-files-in-dir doc-dir))


(def spacedocs (pmap io/fp->*spacedoc-m edn-files))


(println (ex-data (:e (first (filter exc/failure? spacedocs)))))
;; (println (:e (first (filter exc/failure? spacedocs))))


(io/export-graph-svg "graph.svg" (viz/build-graph (apply data/node-relations (map deref spacedocs))))

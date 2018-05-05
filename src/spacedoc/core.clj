(ns spacedoc.core
  (:gen-class)
  (:require [cats.core :as m]
            [spacedoc.data :as data]
            [clojure.string :refer [join]]
            [spacedoc.io :as io]
            [cats.monad.exception :as exc]
            [clojure.core.reducers :as r]))


(def ops
  [["-i" "--input DIRECTORY" "Inpu directory"
    :validate [io/directory? "Input isn't a directory."]]
   ["-h" "--help"]])


(defn fmt-err
  [err]
  (let [{:keys [cause data]} (Throwable->map err)]
    (format "Cause: %s\nData: %s\n" cause data)))


(defn -main [& args]
  (let [e (m/mlet
           [input (io/args->spacedocs-m args ops)]
           (eduction (filter exc/failure?)
                     (map m/extract)
                     (map fmt-err)
                     input))]
    (if-let [err-msg (cond (exc/failure? e) (fmt-err (m/extract e))
                           (seq e) (join "\n\n" e))]
      (do (println err-msg)
          (System/exit 2))
      (println "All good!"))))

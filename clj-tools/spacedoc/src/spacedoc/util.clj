(ns spacedoc.util
  (:gen-class)
  (:require [cats.core :as m]
            [clojure.string :refer [join]]
            [cats.monad.exception :as exc]
            [spacedoc.io :as sio]
            [clojure.core.reducers :as r]))


(defn err->msg
  [err]
  (let [{:keys [cause data]} (Throwable->map err)]
    (println-str
     (join \newline
           (interleave
            (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                 ["File" "Cause" "Data"])
            [(or (:file data) "<none>") cause (or (:problems data) data)])))))

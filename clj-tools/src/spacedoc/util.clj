(ns spacedoc.util
  (:gen-class)
  (:require [cats.core :as m]
            [clojure.string :refer [join]]
            [cats.monad.exception :as exc]
            [spacedoc.io :as sio]
            [clojure.core.reducers :as r]))


(defn fmt-err
  [err]
  (let [{:keys [cause data]} (Throwable->map err)]
    (println-str
     (join \newline
           (interleave
            (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                 ["File" "Cause" "Data"])
            [(or (:file data) "<none>") cause (or (:problems data) data)])))))


(defn spacedocs-exc->spacedocs
  "Unwraps spacedocs from `cats.monad.exception` and returns it.
  If error has occurred it will be outputted with `sio/exit-err`"
  [spacedocs-exc]
  (let [out-exc (m/bind spacedocs-exc m/sequence)
        out (m/extract out-exc)]
    (when (exc/failure? out-exc)
      (sio/exit-err (fmt-err out)))
    out))


(defn fail
  [msg dat]
  (exc/failure (ex-info msg dat)))

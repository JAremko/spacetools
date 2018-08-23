(ns spacedoc.util
  (:require [clojure.string :refer [join]]
            [cats.monad.exception :as exc]))


(defn err->msg
  [err]
  (let [{:keys [cause data]} (Throwable->map err)]
    (join \newline
          (interleave
           (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                ["Cause" "File" "Data"])
           [cause
            (or (:file data) "<none>")
            (or (:problems data) (or data "<none>"))]))))


(defn foldable?
  [col]
  (or (vector? col)
      (map? col)
      (set? col)))


(defn fail
  ([msg] (exc/failure (Exception. (str msg))))
  ([msg dat] (exc/failure (ex-info msg dat))))

(ns spacedoc.util
  (:require [clojure.string :refer [join]]))


(defn err->msg
  [err]
  (let [{:keys [cause data]} (Throwable->map err)]
    (println-str
     (join \newline
           (interleave
            (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                 ["Cause" "File" "Data"])
            [cause (or (:file data) "<none>") (or (:problems data) data)])))))


(defn foldable?
  [col]
  (or (vector? col)
      (map? col)
      (set? col)))

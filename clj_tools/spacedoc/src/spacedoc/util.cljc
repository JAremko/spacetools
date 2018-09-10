(ns spacedoc.util
  "General utilities."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join]]))


(defn err->msg
  [^Throwable err]
  (let [{:keys [cause data]} (Throwable->map err)]
    (join \newline
          (interleave
           (map #(apply str % ": " (repeat (- 78 (count %)) "="))
                ["Cause" "File" "Data"])
           [cause
            (or (:file data) "<none>")
            (or (:problems data) (seq data) "<none>")]))))


(defn foldable?
  [col]
  (or (vector? col)
      (map? col)
      (set? col)))


(def unqualified-ident? (complement qualified-ident?))


(defn unqualify
  [symbol-or-keyword]
  {:pre [(ident? symbol-or-keyword)]}
  (if (unqualified-ident? symbol-or-keyword)
    symbol-or-keyword
    (when-let [name (name symbol-or-keyword)]
      (if (keyword? symbol-or-keyword)
        (keyword name)
        (symbol name)))))

(ns spacedoc.util
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


(defn map-spec->keys
  [spec-or-key]
  {:pre [((some-fn s/spec? keyword?) spec-or-key)]}
  (let [spec (if (keyword? spec-or-key)
               (s/get-spec spec-or-key)
               spec-or-key)]
    (some->> spec
             (s/describe)
             (reduce #(if (vector? %2) (concat %1 %2) %1) [])
             (vec))))


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

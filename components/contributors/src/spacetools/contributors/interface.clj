(ns spacetools.contributors.interface
  (:require [spacetools.contributors.core :as core]))

;; delegate to the implementations...
(defn add-two [x]
  (core/add-two x))

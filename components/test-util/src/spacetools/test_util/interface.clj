(ns spacetools.test-util.interface
  (:require [spacetools.test-util.core :as tu]))


(defn samples [base-sample-count] (tu/samples base-sample-count))
(defn make-f-spec-reper [re-spec f f-name]
  (tu/make-f-spec-reper re-spec f f-name))

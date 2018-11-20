(ns spacetools.spacedoc-org.interface
  (:require [spacetools.spacedoc-org.core :as core]))

(defn sdn->org
  [node]
  (core/sdn->org node))

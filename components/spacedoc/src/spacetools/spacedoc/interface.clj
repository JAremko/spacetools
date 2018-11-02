(ns spacetools.spacedoc.interface
  (:require [spacetools.spacedoc.org :as org]))

(defn sdn->org
  [node]
  (org/sdn->org node))

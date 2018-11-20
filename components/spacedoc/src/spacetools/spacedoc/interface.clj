(ns spacetools.spacedoc.interface
  (:require [spacetools.spacedoc.node :as n]))

(def inline-container-tags n/inline-container-tags)
(def inline-leaf-tags n/inline-leaf-tags)
(def block-tags n/block-tags)
(def headlines-tags n/headlines-tags)
(defn section [& children] (apply n/section children))
(defn headline [value & children] (apply n/headline value children))
(defn unordered-list [& items] (apply n/unordered-list items))
(defn text [value] (n/text value))
(defn link [link & children] (apply n/link link children))
(defn line-break [] (n/line-break))

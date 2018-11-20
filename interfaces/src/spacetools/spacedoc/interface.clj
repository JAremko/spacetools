(ns spacetools.spacedoc.interface)

(defn sdn->org [node])

(def block-tags)
(def headlines-tags)
(def inline-container-tags)
(def inline-leaf-tags)
(defn headline [value & children])
(defn line-break [])
(defn section [& children])
(defn text [value])
(defn unordered-list [& items])
(defn link [link & children])
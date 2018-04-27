(ns spacedoc.data
  (:require [clojure.spec.alpha :as s]))


(load "data_spec")


(def all-tags #{:bold
                :center
                :kbd
                :example
                :fixed-width
                :headline
                :headline-level-1
                :headline-level-2
                :headline-level-3
                :headline-level-4
                :headline-level-5
                :headline-level-6
                :todo
                :description
                :body
                :italic
                :list-item
                :item-children
                :item-tag
                :keyword
                :line-break
                :org-file-path
                :embeddable-file-path
                :file-path
                :embeddable-web-link
                :web-link
                :paragraph
                :feature-list
                :plain-list
                :plain-text
                :quote
                :section
                :src
                :strike-through
                :subscript
                :superscript
                :table
                :table-cell
                :table-row
                :root
                :underline
                :verbatim
                :verse})

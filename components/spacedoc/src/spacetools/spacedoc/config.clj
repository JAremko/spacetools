(ns spacetools.spacedoc.config
  "SDN manipulation utilities."
  (:require [clojure.string :as str]))


;;;; General

(def seps
  #{\! \? \: \' \( \) \; \{ \} \, \. \\ \“ \‘ \’ \” \newline \space \tab})

(def seps-right (disj seps \) \” \’))

(def seps-left  (disj seps \( \“ \‘))

(def text-rep-map
  {#"\r+" ""
   #"\t" " "
   #"[ ]{2,}" " "
   ;; Key-binding
   #"(?i)(\p{Blank}|\p{Blank}\p{Punct}+|^)(k){1}ey[-_]*binding(s{0,1})(\p{Blank}|\p{Punct}+\p{Blank}|$)" "$1$2ey binding$3$4"})

(def custom-id-link-rep-map {#"(?i)([-]+|^|#)key(?:[_]*|-{2,})binding([s]{0,1})([-]+|$)" "$1key-binding$2$3"})

(def link-type->prefix {:file "file:"
                        :http "http://"
                        :https "https://"
                        :custom-id "#"
                        :ftp "ftp://"})

(def link-types (-> link-type->prefix keys set))

(def max-headline-depth 5)

(def toc-max-depth 4)

(def toc-hl-val (format "Table of Contents%s:TOC_%s_gh:noexport:"
                        (str/join (repeatedly 21 (constantly " ")))
                        toc-max-depth))


;;;; Org

(def emphasis-tokens {:bold "*"
                      :italic "/"
                      :verbatim "="
                      :underline "_"
                      :kbd "~"  ;; Called code in "the classic ORG".
                      :strike-through "+"})

(def begin-end-indentation 2)

(def table-indentation 0)

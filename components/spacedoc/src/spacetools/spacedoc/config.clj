(ns spacetools.spacedoc.gnr-config
  "SDN manipulation utilities."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]))


(def default-configs
  {:general
   {:separators-rigth #{\space \! \' \( \tab \newline \, \. \‘ \: \; \{ \“ \\ \} \?}
    :separators-left #{\space \! \' \tab \) \newline \, \. \’ \: \; \{ \\ \” \} \?}
    :text-replacement-map {#"\r+" ""
                           #"\t" " "
                           #"[ ]{2,}" " "
                           ;; Key-binding
                           #"(?i)(\p{Blank}|\p{Blank}\p{Punct}+|^)(k){1}ey[-_]*binding(s{0,1})(\p{Blank}|\p{Punct}+\p{Blank}|$)" "$1$2ey binding$3$4"}
    :custom-id-link-replacement-map {#"(?i)([-]+|^|#)key(?:[_]*|-{2,})binding([s]{0,1})([-]+|$)" "$1key-binding$2$3"}
    :link-type->prefix {:file "file:"
                        :http "http://"
                        :https "https://"
                        :custom-id "#"
                        :ftp "ftp://"}
    :max-headline-depth 5
    :toc-max-depth 4
    :toc-headline-template "Table of Contents                     :TOC_%s_gh:noexport:"}
   :orgify
   {:emphasis-tokens {:bold "*"
                      :italic "/"
                      :verbatim "="
                      :underline "_"
                      :kbd "~"  ;; Called code in the "classic" org.
                      :strike-through "+"}
    :begin-end-indentation 2
    :table-indentation 0}})


(def configs default-configs)


(defn-spec gnr-conf (s/map-of keyword? any?)
  [ks (s/coll-of keyword?)]
  (get-in configs (concat [:general] ks)))


(defn-spec orgify-conf (s/map-of keyword? any?)
  [ks (s/coll-of keyword?)]
  (get-in configs (concat [:orgify] ks)))


;;;; General

(def seps-right (gnr-conf [:separators-rigth]))

(def seps-left  (gnr-conf [:separators-left]))

(def text-rep-map (gnr-conf [:text-replacemant-map]))

(def custom-id-link-rep-map (gnr-conf [:custom-id-link-replacement-map]))

(def link-type->prefix (gnr-conf [:link-type->prefix]))

(def link-types (-> link-type->prefix keys set))

(def max-headline-depth (gnr-conf [:max-headline-depth]))

(def toc-max-depth (gnr-conf [:toc-max-depth]))

(def toc-hl-val (format (gnr-conf [:toc-headline-template]) toc-max-depth))


;;;; Org

(def emphasis-tokens (orgify-conf [:emphasis-tokens]))

(def begin-end-indentation (orgify-conf [:begin-end-indentation]))

(def table-indentation (orgify-conf [:table-indentation]))

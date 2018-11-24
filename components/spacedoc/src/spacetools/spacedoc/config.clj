(ns spacetools.spacedoc.config
  "SDN manipulation utilities."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]))


(def config-file-name "sdn_config.edn")

(def default-config
  {:global
   {:separators-rigth #{\space \! \' \( \tab \newline \, \. \‘ \: \; \{ \“ \\ \} \?}
    :separators-left #{\space \! \' \tab \) \newline \, \. \’ \: \; \{ \\ \” \} \?}
    :text-replacement-map {"\\r+" ""
                           "\\t" " "
                           "[ ]{2,}" " "
                           ;; Key-binding
                           "(?i)(\\p{Blank}|\\p{Blank}\\p{Punct}+|^)(k){1}ey[-_]*binding(s{0,1})(\\p{Blank}|\\p{Punct}+\\p{Blank}|$)" "$1$2ey binding$3$4"}
    :custom-id-link-replacement-map {"(?i)([-]+|^|#)key(?:[_]*|-{2,})binding([s]{0,1})([-]+|$)" "$1key-binding$2$3"}
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


(defn- sync-config
  [cfg cfg-f]
  (io! (let [synced-cfg (if (.exists (io/file cfg-f))
                          (edn/read-string (slurp cfg-f))
                          cfg)]
         (do (pp/pprint synced-cfg (clojure.java.io/writer cfg-f))
             cfg))))


(def config (sync-config default-config config-file-name))


(defn- glob-conf
  [ks]
  (get-in config (concat [:global] ks)))


(defn- orgify-conf
  [ks]
  (get-in config (concat [:orgify] ks)))


(defn- map-keys
  [f map]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} map))


;;;; General

(def seps-right (glob-conf [:separators-rigth]))

(def seps-left  (glob-conf [:separators-left]))

(def text-rep-map (map-keys re-pattern (glob-conf [:text-replacement-map])))

(def custom-id-link-rep-map (map-keys
                             re-pattern
                             (glob-conf [:custom-id-link-replacement-map])))

(def link-type->prefix (glob-conf [:link-type->prefix]))

(def link-types (-> link-type->prefix keys set))

(def max-headline-depth (glob-conf [:max-headline-depth]))

(def toc-max-depth (glob-conf [:toc-max-depth]))

(def toc-hl-val (format (glob-conf [:toc-headline-template]) toc-max-depth))


;;;; Org

(def emphasis-tokens (orgify-conf [:emphasis-tokens]))

(def begin-end-indentation (orgify-conf [:begin-end-indentation]))

(def table-indentation (orgify-conf [:table-indentation]))

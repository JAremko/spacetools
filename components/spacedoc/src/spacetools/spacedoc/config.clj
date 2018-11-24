(ns spacetools.spacedoc.config
  "SDN manipulation utilities."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]))


(def config-file-name "sdn_config.edn")

(def default-config
  {:text/separators-rigth #{\space \! \' \( \tab \newline \, \. \‘ \: \; \{ \“ \\ \} \?}
   :text/separators-left #{\space \! \' \tab \) \newline \, \. \’ \: \; \{ \\ \” \} \?}
   :text/replacement-map {"\\r+" ""
                          "\\t" " "
                          "[ ]{2,}" " "
                          ;; Key-binding
                          "(?i)(\\p{Blank}|\\p{Blank}\\p{Punct}+|^)(k){1}ey[-_]*binding(s{0,1})(\\p{Blank}|\\p{Punct}+\\p{Blank}|$)" "$1$2ey binding$3$4"}
   :link/custom-id-replacement-map {"(?i)([-]+|^|#)key(?:[_]*|-{2,})binding([s]{0,1})([-]+|$)" "$1key-binding$2$3"}
   :link/type->prefix {:file "file:"
                       :http "http://"
                       :https "https://"
                       :custom-id "#"
                       :ftp "ftp://"}
   :headline/max-depth 5
   :org/toc-max-depth 4
   :org/toc-template "Table of Contents                     :TOC_%s_gh:noexport:"
   :org/emphasis-tokens {:bold "*"
                         :italic "/"
                         :verbatim "="
                         :underline "_"
                         :kbd "~"  ;; Called code in the "classic" org.
                         :strike-through "+"}
   :org/block-indentation 2
   :org/table-indentation 0})

(s/def :text/separators-rigth (s/coll-of string? :kind set?))

(s/def :text/separators-left (s/coll-of string? :kind set?))

(s/def :text/replacement-map (s/map-of string? string?))

(s/def :text/custom-id-replacement-map (s/map-of string? string?))

(s/def :link/type->prefix (s/map-of keyword? string?))

(s/def :headline/max-depth nat-int?)

(s/def :org/toc-max-depth nat-int?)

(s/def :org/toc-template (s/with-gen (s/and string? #(re-matches #".*%s.*" %))
                            #(gen/fmap (fn [[head tail]] (str head "%s" tail))
                                       (gen/tuple
                                        (gen/string-alphanumeric)
                                        (gen/string-alphanumeric)))))

(s/def :org/emphasis-tokens (s/map-of keyword? string?))

(s/def :org/block-indentation pos-int?)

(s/def :org/table-indentation pos-int?)

(s/def ::configs (s/keys :req [:text/separators-rigth
                               :text/separators-left
                               :text/replacement-map
                               :text/custom-id-replacement-map
                               :link/type->prefix
                               :headline/max-depth
                               :org/toc-max-depth
                               :org/toc-template
                               :org/emphasis-tokens
                               :org/block-indentation
                               :org/table-indentation]))

(s/def ::configs-from-file (s/keys :op [:text/separators-rigth
                                        :text/separators-left
                                        :text/replacement-map
                                        :text/custom-id-replacement-map
                                        :link/type->prefix
                                        :headline/max-depth
                                        :org/toc-max-depth
                                        :org/toc-template
                                        :org/emphasis-tokens
                                        :org/block-indentation
                                        :org/table-indentation]))


(defn-spec sync-configs ::configs
  [cfg ::configs cfg-f ::configs-from-file]
  (io! (let [synced-cfg (if (.exists (io/file cfg-f))
                          (merge default-config (edn/read-string (slurp cfg-f)))
                          cfg)]
         (do (pp/pprint synced-cfg (clojure.java.io/writer cfg-f))
             cfg))))


(def configs (sync-configs default-config config-file-name))


(defn- map-keys
  [f map]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} map))


;;;; General

(def seps-right (:text/separators-rigth configs))

(def seps-left  (:text/separators-left configs))

(def text-rep-map (map-keys re-pattern (:text/replacement-map configs)))

(def custom-id-link-rep-map (map-keys
                             re-pattern
                             (:link/custom-id-replacement-map configs)))

(def link-type->prefix (:link/type->prefix configs))

(def link-types (-> link-type->prefix keys set))

(def max-headline-depth (:headline/max-depth configs))

(def toc-max-depth (:org/toc-max-depth configs))

(def toc-hl-val (format (:org/toc-template configs) toc-max-depth))


;;;; Org

(def emphasis-tokens (:org/emphasis-tokens configs))

(def begin-end-indentation (:org/block-indentation configs))

(def table-indentation (:org/table-indentation configs))

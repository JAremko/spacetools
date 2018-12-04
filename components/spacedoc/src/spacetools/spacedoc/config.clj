(ns spacetools.spacedoc.config
  "Global configurations."
  (:require [clojure.edn :as edn]
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
                          "(?:(?i)(\\p{Blank}|\\p{Blank}\\p{Punct}+|^)(k){1}ey)(?:(?:(?i)[-_]*b)| B)(?:(?i)inding)((?i)s{0,1}(?:\\p{Blank}|\\p{Punct}+\\p{Blank}|\\p{Punct}+$|$))" "$1$2ey binding$3"
                          "((?i)k)ey bindingS" "$1ey bindings"}
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

(s/def :text/separators-rigth (s/coll-of char? :kind set?))

(s/def :text/separators-left (s/coll-of char? :kind set?))

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

(s/def :org/block-indentation nat-int?)

(s/def :org/table-indentation nat-int?)

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

(s/def ::overriding-configs (s/keys :op [:text/separators-rigth
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


(def *configs (atom default-config))


(defn-spec valid-configs? boolean?
  [configs ::configs]
  (s/valid? ::configs configs))


(defn-spec valid-overrides? boolean?
  [configs ::overriding-configs]
  (s/valid? ::overriding-configs configs))


(defn-spec override-configs! ::configs
  [overrides ::overriding-configs]
  (swap! *configs merge overrides))


(defn map-keys
  [f map]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} map))


(defn regexp?
  [re-pat]
  (instance? java.util.regex.Pattern re-pat))


;;;; General

(defn-spec seps-right :text/separators-rigth
  []
  (:text/separators-rigth @*configs))


(defn-spec seps-left :text/separators-left
  []
  (:text/separators-left @*configs))


(defn-spec text-rep-map (s/map-of regexp? string?)
  []
  (map-keys re-pattern (:text/replacement-map @*configs)))


(defn-spec custom-id-link-rep-map (s/map-of regexp? string?)
  []
  (map-keys
   re-pattern
   (:link/custom-id-replacement-map @*configs)))


(defn-spec link-type->prefix :link/type->prefix
  []
  (:link/type->prefix @*configs))


(defn-spec link-types (s/coll-of keyword? :kind set?)
  []
  (-> (link-type->prefix) keys set))


(defn-spec max-headline-depth :headline/max-depth
  []
  (:headline/max-depth @*configs))


(defn-spec toc-max-depth :org/toc-max-depth
  []
  (:org/toc-max-depth @*configs))


(defn-spec toc-hl-val string?
  []
  (format (:org/toc-template @*configs) (toc-max-depth)))


;;;; Org

(defn-spec emphasis-tokens :org/emphasis-tokens
  []
  (:org/emphasis-tokens @*configs))


(defn-spec begin-end-indentation :org/block-indentation
  []
  (:org/block-indentation @*configs))


(defn-spec table-indentation :org/table-indentation
  []
  (:org/table-indentation @*configs))

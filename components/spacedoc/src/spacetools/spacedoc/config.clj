(ns spacetools.spacedoc.config
  "Global configurations."
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]))


(def config-file-name
  "File name of configurations overrides file."
  "sdn_overrides.edn")


(def default-config
  "Default configurations."
  ;; Allowed tags of documentation files.
  #::{:valid-tags
      {"programming" "Programming languages"
       "music" "Music"
       "file tree" "File trees"
       "font" "Fonts"
       "dsl" "Domain-specific"
       "js" "JavaScript"
       "e-mail" "E-mail"
       "distribution" "Distributions"
       "spacemacs" "Spacemacs"
       "os" "Operating systems"
       "chat" "Chats"
       "util" "Utilities"
       "framework" "Frameworks"
       "markup" "Markup languages"
       "tag" "Tagging"
       "reader" "Readers"
       "i18n" "internationalization"
       "pairing" "Pair programming"
       "web service" "Web services"
       "versioning" "Source control"
       "fun" "Fun"
       "script" "Scripting"
       "imperative" "Imperative"
       "general" "General-purpose"
       "pure" "Purely functional"
       "emacs" "Emacs"
       "vim" "Vim"
       "completion" "Completion"
       "tool" "Tools"
       "misc" "Miscellaneous"
       "lisp" "Lisp dialects"
       "layer" "All layers"
       "theme" "Themes"
       "multi-paradigm" "Multi-paradigm"
       "checker" "Checkers"
       "uncategorized" "README.org files that need proper tags"
       }
      :layers-org-query
      ["chat"
       "checker"
       "completion"
       "e-mail"
       "file tree"
       "font"
       "emacs"
       "framework"
       "fun"
       "i18n"
       "markup"
       "misc"
       "music"
       "os"
       "pairing"
       "reader"
       {"programming" ["util"
                       "dsl"
                       "lisp"
                       "script"
                       {"general" ["imperative"
                                   "multi-paradigm"
                                   "js"
                                   ]}
                       "pure"
                       ]}
       "versioning"
       {"spacemacs" ["distribution"
                     "util"
                     ]}
       "tag"
       "theme"
       "tool"
       "vim"
       "web service"
       "uncategorized"
       ]
      :text-separators-rigth #{\space \! \' \( \tab \newline \, \. \‘ \: \; \{ \“ \\ \} \?}
      :text-separators-left #{\space \! \' \tab \) \newline \, \. \’ \: \; \{ \\ \” \} \?}
      :text-replacement-map {"\\r+" ""
                             "\\t" " "
                             "[ ]{2,}" " "
                             ;; Key-binding
                             "(?:(?i)(\\p{Blank}|\\p{Blank}\\p{Punct}+|^)(k){1}ey)(?:(?:(?i)[-_]*b)| B)(?:(?i)inding)((?i)s{0,1}(?:\\p{Blank}|\\p{Punct}+\\p{Blank}|\\p{Punct}+$|$))" "$1$2ey binding$3"
                             "((?i)k)ey bindingS" "$1ey bindings"}
      :link-custom-id-replacement-map {"(?i)([-]+|^|#)key(?:[_]*|-{2,})binding([s]{0,1})([-]+|$)" "$1key-binding$2$3"}
      :link-type->prefix {:file "file:"
                          :http "http://"
                          :https "https://"
                          :custom-id "#"
                          :ftp "ftp://"}
      :headline-max-depth 5
      :org-toc-max-depth 4
      :org-toc-template "Table of Contents                     :TOC_%s_gh:noexport:"
      :org-emphasis-tokens {:bold "*"
                            :italic "/"
                            :verbatim "="
                            :underline "_"
                            :kbd "~"  ;; Called code in the "classic" org.
                            :strike-through "+"}
      :org-block-indentation 2
      :org-table-indentation 0})


(s/def ::valid-tags (s/map-of (s/and string?
                                     (complement #(str/includes? % "|")))
                              string?))

(s/def ::layers-org-query
  (s/coll-of
   (s/or :join (s/map-of string? ::layers-org-query
                         :count 1)
         :select string?)
   :kind vector?
   :min-count 1))

(s/def ::text-separators-rigth (s/coll-of char? :kind set?))

(s/def ::text-separators-left (s/coll-of char? :kind set?))

(s/def ::text-replacement-map (s/map-of string? string?))

(s/def ::link-custom-id-replacement-map (s/map-of string? string?))

(s/def ::link-type->prefix (s/map-of keyword? string?))

(s/def ::headline-max-depth nat-int?)

(s/def ::org-toc-max-depth nat-int?)

(s/def ::org-toc-template (s/and string? #(re-matches #".*%s.*" %)))

(s/def ::org-emphasis-tokens (s/map-of keyword? string?))

(s/def ::org-block-indentation nat-int?)

(s/def ::org-table-indentation nat-int?)

(s/def ::configs (s/keys :req [::valid-tags
                               ::layers-org-query
                               ::text-separators-rigth
                               ::text-separators-left
                               ::text-replacement-map
                               ::link-custom-id-replacement-map
                               ::link-type->prefix
                               ::headline-max-depth
                               ::org-toc-max-depth
                               ::org-toc-template
                               ::org-emphasis-tokens
                               ::org-block-indentation
                               ::org-table-indentation]))

(s/def ::overriding-configs (s/keys :op [::valid-tags
                                         ::layers-org-query
                                         ::text-separators-rigth
                                         ::text-separators-left
                                         ::text-replacement-map
                                         ::link-custom-id-replacement-map
                                         ::link-type->prefix
                                         ::headline-max-depth
                                         ::org-toc-max-depth
                                         ::org-toc-template
                                         ::org-emphasis-tokens
                                         ::org-block-indentation
                                         ::org-table-indentation]))


(def *configs
  "Configuration atom."
  (atom default-config))


(defn-spec valid-overrides? boolean?
  "Return true if CONFIGS is valid override configuration.
Same as `valid-configs?` but all elements of the CONFIGS map are optional."
  [configs any?]
  (s/valid? ::overriding-configs configs))


(defn-spec override-configs! ::configs
  "Apply OVERRIDES to the current  configuration atom."
  [overrides ::overriding-configs]
  (swap! *configs merge overrides))


(defn-spec regexp? boolean?
  "Return true if RE-PAT is a regex pattern."
  [re-pat any?]
  (instance? java.util.regex.Pattern re-pat))


;;;; General

(defn-spec valid-tags ::valid-tags
  "Return set of tag->description of valid tags."
  []
  (::valid-tags @*configs))


(defn-spec layers-org-query ::layers-org-query
  "Return query of layers.org file. See `::layers-org-query`."
  []
  (::layers-org-query @*configs))


(defn-spec seps-left ::text-separators-left
  "Return left separators."
  []
  (::text-separators-left @*configs))


(defn-spec seps-right ::text-separators-rigth
  "Return right separators."
  []
  (::text-separators-rigth @*configs))


(defn-spec text-rep-map (s/map-of regexp? string?)
  "Return text replacement map."
  []
  (reduce-kv (fn [m k v] (assoc m (re-pattern k) v))
             {}
             (::text-replacement-map @*configs)))


(defn-spec custom-id-link-rep-map (s/map-of regexp? string?)
  "Return custom-id links replacement map."
  []
  (reduce-kv (fn [m k v] (assoc m (re-pattern k) v))
             {}
             (::link-custom-id-replacement-map @*configs)))


(defn-spec link-type->prefix ::link-type->prefix
  "Given link type return corresponding prefix."
  []
  (::link-type->prefix @*configs))


(defn-spec link-types (s/coll-of keyword? :kind set?)
  "Return all types of links."
  []
  (-> (link-type->prefix) keys set))


(defn-spec max-headline-depth ::headline-max-depth
  "Return max depth(level) that headline can have."
  []
  (::headline-max-depth @*configs))


(defn-spec toc-max-depth ::org-toc-max-depth
  "Return depth after which TOC entries cut-off."
  []
  (::org-toc-max-depth @*configs))


(defn-spec toc-hl-val string?
  "Return standard headline for a TOC."
  []
  (format (::org-toc-template @*configs) (toc-max-depth)))


;;;; Orgify

(defn-spec emphasis-tokens ::org-emphasis-tokens
  "Return emphasis tokens of org-mode."
  []
  (::org-emphasis-tokens @*configs))


(defn-spec begin-end-indentation ::org-block-indentation
  "Return indentation of org-mode BEGIN-END blocks."
  []
  (::org-block-indentation @*configs))


(defn-spec table-indentation ::org-table-indentation
  "Return indentation of org-mode tables."
  []
  (::org-table-indentation @*configs))

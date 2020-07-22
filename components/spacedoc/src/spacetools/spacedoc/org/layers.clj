(ns spacetools.spacedoc.org.layers
  "layers.org generator."
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [difference]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :refer [flatten-headline]]))


(defn-spec root->description (s/nilable :spacetools.spacedoc.node/headline)
  "Extracts description of a SDN document."
  [root :spacetools.spacedoc.node/root]
  (->> root
       :children
       (filter (partial s/valid? :spacetools.spacedoc.node.meta/description))
       first))


(defn-spec capitalize-first string?
  "Like str/capitalize but doesn't force rest of characters into low-case."
  [str string?]
  (str/join (into (rest str) ((fnil str/upper-case "") (first str)))))


(def missing-source-link-text
  "Text for missing source link placeholder."
  "<layer link is missing>")

(def missing-source-link-ph
  "Placeholder for missing source link."
  (n/text missing-source-link-text))

(def missing-description-text
  "Text for missing description placeholder."
  "<README.org of the layer misses or has invalid \"Description\".>")

(def missing-description-ph
  "Placeholder for missing description."
  (->> missing-description-text
       n/text
       n/bold
       n/paragraph
       n/section
       (n/headline "<Placeholder>")
       vector))


(defn-spec describe :spacetools.spacedoc.node/headline
  "Returns headline describing a SDN document."
  [root :spacetools.spacedoc.node/root]
  (apply
   n/headline
   (-> root
       (:title)
       (str/trim)
       (str/replace-first #"\s+layer$" "")
       (capitalize-first))
   (into (-> (if-let [src (:source root)]
               (n/link (str "file:" src) (n/text src))
               missing-source-link-ph)
             n/paragraph
             (n/section (n/paragraph (n/line-break)))
             vector)
         (if-let [description (root->description root)]
           (:children (flatten-headline 1 description))
           missing-description-ph))))


(defn-spec fmt-invalid-tag :spacetools.spacedoc.node.val-spec/non-blank-string
  "Formats TEXT-KEY for display as an invalid category tag value."
  [text-key string?]
  (->> text-key
       (format "<\"%s\" invalid tag>")
       str/upper-case))


(defn-spec wrap-in-hl :spacetools.spacedoc.node/headline
  "Wraps CHILDREN seq into headline with text val from `cfg/valid-tags`"
  [text-key :spacetools.spacedoc.node.val-spec/non-blank-string
   children (s/coll-of :spacetools.spacedoc.node/headline-child
                       :min-count 0
                       :distinct true)]
  (apply n/headline
         (get (cfg/valid-tags) text-key (fmt-invalid-tag text-key))
         children))


(defn-spec merge-same-hls (s/coll-of :spacetools.spacedoc.node/headline
                                     :kind vector?
                                     :distinct true)
  "Merge headlines with the same `:value`."
  [hls (s/coll-of :spacetools.spacedoc.node/headline)]
  (->> hls
       (r/reduce
        (fn [acc {:keys [value children] :as hl}]
          (if (acc value)
            (update-in acc [value :children] #(into % children))
            (assoc acc value hl)))
        {})
       vals
       vec))


(defn-spec query-node? boolean?
  "Returns true if X is a query node."
  [x any?]
  (s/valid? :spacetools.spacedoc.config/layers-org-query x))


(defn-spec query-node-parent? boolean?
  "Returns true if X is a query node that has children."
  [x any?]
  ((every-pred query-node? #(-> % first val seq)) x))


(defn-spec query-fragment->tag string?
  "Given query node fragment return it's tag."
  [fragment (s/or :q-node :spacetools.spacedoc.config/layers-org-query
                  :tag #((cfg/valid-tags) %))]
  (if (query-node? fragment) (ffirst fragment)
      fragment))


(s/def :spacetools.spacedoc.org.layers.shaper/shaped
  (s/nilable :spacetools.spacedoc.node/headline))
(s/def :spacetools.spacedoc.org.layers.shaper/leftover
  (s/coll-of :spacetools.spacedoc.node/root
             :distinct true))
(s/def ::shaped-ret-val
  (s/keys :req-un [:spacetools.spacedoc.org.layers.shaper/shaped
                   :spacetools.spacedoc.org.layers.shaper/leftover]))


(def query-no-match
  "Placeholder for shaped docs when query doesn't match any."
  (n/section (n/paragraph (n/text "<No matched docs>"))))


(defn-spec layers-query-shaper ::shaped-ret-val
  "Fills QUERY shape with data from DOCS documentation files."
  [docs (s/coll-of :spacetools.spacedoc.node/root
                   :distinct true)
   query :spacetools.spacedoc.config/layers-org-query]
  (let [all-docs-v (volatile! (set docs))]
    {:shaped ((fn self [rest-of-matching-docs query-node]
                (let [q-tag (query-fragment->tag query-node)]
                  (when-let [matching-docs
                             (seq (filter #(and
                                            ;; When the document's tags
                                            ;; contain current query tag.
                                            ((:tags %) q-tag)
                                            ;; And this document haven't
                                            ;; been added yet.
                                            (@all-docs-v %))
                                          rest-of-matching-docs))]
                    ((fnil wrap-in-hl nil [query-no-match])
                     q-tag
                     (seq (if (query-node-parent? query-node)
                            (->> query-node
                                 first
                                 val
                                 (map (partial self matching-docs))
                                 (remove nil?)
                                 merge-same-hls
                                 (sort-by (comp str/upper-case :value)))
                            (do (vswap! all-docs-v difference matching-docs)
                                (->> matching-docs
                                     (sort-by (comp str/upper-case :title))
                                     (mapv describe)))))))))
              docs query)
     :leftover @all-docs-v}))


(def layers-org-autogen-note
  "Note about layers.org being auto-generated."
  (n/headline "THIS FILE IS AUTO GENERATED"
              (->> (str "Don't edit it directly.\n"
                        "See [["
                        "https://github.com/syl20bnr/spacemacs"
                        "/blob/develop/CONTRIBUTING.org#readmeorg-tags"
                        "][\"README.org tags\" section of"
                        " CONTRIBUTING.org for the instructions]].")
                   n/text
                   n/paragraph
                   n/section)))

(def no-layer-readme-files-ph
  "Placeholder for layer readme.org files."
  (n/todo "No README.org files with \"layer\" tag."))

(def layers-org-title-text
  "Title text of the layers.org file."
  "Configuration layers")

(def skipped-layers-text
  "Text for the skipped layers headline.
  NOTE: I extracted even short text literals into defs simply because
  it should help with testing."
  "Skipped layers:")

(defn-spec layers-sdn :spacetools.spacedoc.node/root
  "Create layers.org from a seq of documentation files."
  [docs (s/coll-of :spacetools.spacedoc.node/root)]
  (let [{shape :shaped
         rest-docs :leftover} (layers-query-shaper docs (cfg/layers-org-query))]
    (apply n/root layers-org-title-text #{}
           (into [layers-org-autogen-note]
                 (or (seq (into (:children shape [] #_ "<= ensures order")
                                (when (seq rest-docs)
                                  (->> rest-docs
                                       (sort-by (comp str/lower-case :title))
                                       (map describe)
                                       (apply n/headline skipped-layers-text)
                                       vector))))
                     [no-layer-readme-files-ph])))))

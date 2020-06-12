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
  (str/join (into (rest str) (str/upper-case (first str)))))


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
               (n/text "<layer link is missing>"))
             n/paragraph
             (n/section (n/paragraph (n/line-break)))
             vector)
         (if-let [description (root->description root)]
           (:children (flatten-headline 1 description))
           (->> "README.org of the layer misses or has invalid \"Description\"."
                n/text
                n/bold
                n/paragraph
                n/section
                (n/headline "Placeholder")
                vector)))))


(defn-spec layers-query-shaper map?
  "Fills QUERY shape with data from DOCS documentation files."
  [docs spacetools.spacedoc.node/root
   query :spacetools.spacedoc.config/layers-org-query]
  (let [wrap-in-hl
        (fn [text children] (apply n/headline
                                  (get (cfg/valid-tags)
                                       text
                                       (->> text
                                            (format "<\"%s\" invalid tag>")
                                            str/upper-case))
                                  children))

        merge-same-hls
        (fn [hls]
          (->> hls
               (r/reduce
                (fn [acc {:keys [value children] :as hl}]
                  (if (acc value)
                    (update-in acc [value :children] #(into children %))
                    (assoc acc value hl)))
                {})
               vals
               vec))

        all-docs-v (volatile! (set docs))

        walker
        (fn self [ds node]
          (let [parent-node (map? node)
                query-tag (if parent-node
                            (ffirst node)
                            node)]
            (if-not ((cfg/valid-tags) query-tag)
              (throw (ex-info "Query has invalid tag" {:tag query-tag}))
              (when-let [matching-docs (->> ds
                                            (filter
                                             #(and
                                               ;; When the document's tags
                                               ;; contain current query tag.
                                               ((:tags %) query-tag)
                                               ;; And this document haven't
                                               ;; been added yet.
                                               (@all-docs-v %)))
                                            seq)]
                (wrap-in-hl query-tag
                            (if parent-node
                              (->> node
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

        shaped (walker docs query)]
    {:shaped shaped :leftover @all-docs-v}))


(defn-spec layers-sdn :spacetools.spacedoc.node/root
  "Create layers.org from a seq of documentation files."
  [docs (s/coll-of :spacetools.spacedoc.node/root)]
  (let [{shape :shaped
         rest-docs :leftover} (layers-query-shaper docs (cfg/layers-org-query))]
    (apply n/root "Configuration layers" #{}
           (conj (or (-> shape
                         (get :children)
                         (into (some->> rest-docs
                                        (filter #(contains? (:tags %) "layer"))
                                        seq
                                        (sort-by (comp str/lower-case :title))
                                        (map describe)
                                        (apply n/headline "Skipped layers:")
                                        vector))
                         seq)
                     (list (n/todo "No README.org files with \"layer\" tag.")))
                 ;; TODO: Make this configurable:
                 (->> (str "Don't edit it directly.\n"
                           "See [["
                           "https://github.com/syl20bnr/spacemacs"
                           "/blob/develop/CONTRIBUTING.org#readmeorg-tags"
                           "][\"README.org tags\" section of"
                           " CONTRIBUTING.org for instructions]].")
                      n/text
                      n/paragraph
                      n/section
                      (n/headline "THIS FILE IS AUTO GENERATED"))))))

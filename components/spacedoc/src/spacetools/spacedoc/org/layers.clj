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
  [node :spacetools.spacedoc.node/root]
  (->> node
       :children
       (filter (partial s/valid? :spacetools.spacedoc.node.meta/description))
       first))


(defn-spec capitalize-first string?
  [str string?]
  (str/join (into (rest str) (str/upper-case (first str)))))


(defn-spec describe :spacetools.spacedoc.node/headline
  [node :spacetools.spacedoc.node/root]
  (apply
   n/headline
   (-> node
       (:title)
       (str/trim)
       (str/replace-first #"\s+layer$" "")
       (capitalize-first))
   (into (-> (if-let [src (:source node)]
               (n/link (str "file:" src) (n/text src))
               (n/text "<layer link is missing>"))
             n/paragraph
             (n/section (n/paragraph (n/line-break)))
             vector)
         (if-let [description (root->description node)]
           (:children (flatten-headline 1 description))
           (->> "README.org of the layer misses or has invalid \"Description\"."
                n/text
                n/bold
                n/paragraph
                n/section
                (n/headline "placeholder")
                vector)))))


(defn merge-same-hls
  [hls]
  (->> hls
       (r/reduce
        (fn [acc {:keys [value children] :as hl}]
          (if (acc value)
            (update-in acc [value :children] #(into children %))
            (assoc acc value hl)))
        {})
       vals
       vec))

#_ (merge-same-hls #{
                     (n/headline "foo" (n/section (n/key-word "1" "foo")))
                     (n/headline "bar" (n/section (n/key-word "2" "foo")))
                     (n/headline "foo" (n/section (n/key-word "3" "foo")))
                     })


;; TODO Replace TAG validation with SPEC on configs read.
(defn-spec layers-sdn (s/nilable :spacetools.spacedoc.node/root)
  "Create layers.org from a seq of documentation files."
  [docs :spacetools.spacedoc.node/root]
  (let [all-docs-v (volatile! #{})
        walk (fn inner [ds node]
               (let [tag (if (map? node)
                           (ffirst node)
                           node)]
                 (if-not ((cfg/valid-tags) tag)
                   (throw (ex-info "Query has invalid tag" {:tag tag}))
                   (when-let [f-ds (seq (filter #(and ((:tags %) tag)
                                                      (@all-docs-v %))
                                                ds))]
                     (apply n/headline (or ((cfg/valid-tags) tag)
                                           (->> tag
                                                (format "<\"%s\" invalid tag>")
                                                str/upper-case))
                            (if (map? node)
                              (->> node
                                   first
                                   val
                                   (map (partial inner f-ds))
                                   (remove nil?)
                                   merge-same-hls
                                   (sort-by (comp str/upper-case :value)))
                              (do (vswap! all-docs-v difference f-ds)
                                  (->> f-ds
                                       (sort-by (comp str/upper-case :title))
                                       (mapv describe)))))))))]

    (apply n/root "Configuration layers" #{}
           (conj (or
                  (seq
                   (into (->> (cfg/layers-org-query)
                              (walk (vreset! all-docs-v (set docs)))
                              :children)
                         (some->> @all-docs-v
                                  (filter #(contains? (:tags %) "layer"))
                                  seq
                                  (sort-by (comp str/lower-case :title))
                                  (map describe)
                                  (apply n/headline "Skipped layers:")
                                  vector)))
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

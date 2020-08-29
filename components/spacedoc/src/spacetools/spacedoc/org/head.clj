(ns spacetools.spacedoc.org.head
  "Specs and helpers for working with headers of documents.
  TODO: `::toc` needs a custom generator. It will allow
        generative testing of TOC generation/conversion.
  TODO: head nodes also need a generator."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str :refer [join]]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu]))

;;;;  TOC spec and constructor

;; TOC leaf (GitHub style link to a local headline)

(s/def :spacetools.spacedoc.org.head.toc.leaf/tag #{:link})
(s/def :spacetools.spacedoc.org.head.toc.leaf/type #{:custom-id})
(s/def :spacetools.spacedoc.org.head.toc.leaf/path
  :spacetools.spacedoc.node.link/path)
(s/def :spacetools.spacedoc.org.head.toc.leaf/children (s/coll-of ::n/text
                                                                  :kind vector?
                                                                  :min-count 1
                                                                  :max-count 1))
(s/def ::toc-leaf (s/keys :req-un
                          [:spacetools.spacedoc.org.head.toc.leaf/tag
                           :spacetools.spacedoc.org.head.toc.leaf/type
                           :spacetools.spacedoc.org.head.toc.leaf/path
                           :spacetools.spacedoc.org.head.toc.leaf/children]))

;; TOC item children wrapper (item-children)

(s/def :spacetools.spacedoc.org.head.toc.item-children/tag #{:item-children})
(s/def :spacetools.spacedoc.org.head.item-children/children
  (s/alt :parent (s/cat
                  :headline-link
                  ::toc-leaf
                  :sub-headlines-links
                  (s/cat :line-break ::n/line-break
                         :brench (s/+ ::toc-branch)))
         :terminal (s/cat :headline-link ::toc-leaf)))

(s/def ::toc-item-children
  (s/keys :req-un [:spacetools.spacedoc.org.head.toc.item-children/tag
                   :spacetools.spacedoc.org.head.item-children/children]))


;; TOC item (unordered list item)

(s/def :spacetools.spacedoc.org.head.toc.item/tag #{:list-item})
(s/def :spacetools.spacedoc.org.head.toc.item/type #{:unordered})
(s/def :spacetools.spacedoc.org.head.toc.item/bullet #{"- "})
(s/def :spacetools.spacedoc.org.head.toc.item/checkbox nil?)
(s/def :spacetools.spacedoc.org.head.toc.item/children
  (s/coll-of ::toc-item-children :kind vector? :min-count 1))
(s/def ::toc-item (s/keys :req-un
                          [:spacetools.spacedoc.org.head.toc.item/tag
                           :spacetools.spacedoc.org.head.toc.item/type
                           :spacetools.spacedoc.org.head.toc.item/bullet
                           :spacetools.spacedoc.org.head.toc.item/checkbox
                           :spacetools.spacedoc.org.head.toc.item/children]))


;; TOC branch (unordered list)

(s/def :spacetools.spacedoc.org.head.toc.branch/tag #{:plain-list})
(s/def :spacetools.spacedoc.org.head.toc.branch/type #{:unordered})
(s/def :spacetools.spacedoc.org.head.toc.branch/children
  (s/coll-of ::toc-item :kind vector? :min-count 1))
(s/def ::toc-branch
  (s/keys :req-un [:spacetools.spacedoc.org.head.toc.branch/tag
                   :spacetools.spacedoc.org.head.toc.branch/type
                   :spacetools.spacedoc.org.head.toc.branch/children]))


;; TOC items wrapper (section)

(s/def :spacetools.spacedoc.org.head.toc.wrapper/tag #{:section})
(s/def :spacetools.spacedoc.org.head.toc.wrapper/children
  (s/coll-of ::toc-branch :kind vector? :min-count 1))
(s/def ::toc-wrapper
  (s/keys :req-un [:spacetools.spacedoc.org.head.toc.wrapper/tag
                   :spacetools.spacedoc.org.head.toc.wrapper/children]))

;; TOC (headline)

(s/def :spacetools.spacedoc.org.head.toc/tag #{:headline})
(s/def :spacetools.spacedoc.org.head.toc/todo? boolean?)
(s/def :spacetools.spacedoc.org.head.toc/children
  (s/coll-of ::toc-wrapper :kind vector?))
(s/def :spacetools.spacedoc.org.head.toc/value
  (s/with-gen #(= (cfg/toc-hl-val) %)
    #(s/gen (hash-set (cfg/toc-hl-val)))))
(s/def ::toc (s/keys :req-un [:spacetools.spacedoc.org.head.toc/tag
                              :spacetools.spacedoc.org.head.toc/todo?
                              :spacetools.spacedoc.org.head.toc/value
                              :spacetools.spacedoc.org.head.toc/children]))


(defn-spec root->toc (s/nilable ::toc)
  "Generate table of content for ROOT node.
Return nil if ROOT node doesn't have any headlines."
  [{children :children :as root} ::n/root]
  (letfn [(hl->gid-base
            ;; "Returns headline value formatted into GitHub link fragment."
            [headlin]
            (sdu/hl-val->gh-id-base (sdu/fmt-hl-val (:value headlin))))

          (gh-id
            ;; Adds counter to the GitHub style link fragment.
            [gid-bs cnt]
            (if (> cnt 1) (str gid-bs "-" (dec cnt)) gid-bs))

          (up-*gid->count!
            ;; Returns and increments counter associated with headline link
            ;; if the link occurs again.
            ;; NOTE: Link must be unique to refer corresponding headlines but
            ;;       the process of formatting headline into a link sometimes
            ;;       create same links for different headlines. Adding counter
            ;;       sub string to it fixes the problem.
            [*gc hl]
            (let [gid-base (hl->gid-base hl)]
              ((vswap! *gc update gid-base (fnil inc 0)) gid-base)))

          (hl->toc-el
            ;; Converts headline into TOC entry.
            [{:keys [value gh-id children]}]
            (n/unordered-list
             (vec (list* (n/link gh-id (n/text (sdu/fmt-hl-val value)))
                         (when (seq children)
                           (list* (n/line-break) children))))))

          (hls->toc-els
            ;; Converts seq of headlines into TOC entries.
            ;; NOTE: `volatile!` simplifies code because with the counter state
            ;;       threading the code gates much more complex.
            [headlines]
            (let [*gid->count (volatile! {})]
              (map (fn inner [depth hl]
                     (-> hl
                         (assoc :gh-id (gh-id (hl->gid-base hl)
                                              (up-*gid->count! *gid->count hl)))
                         (update :children
                                 #(when (< depth (cfg/toc-max-depth))
                                    (when-let [hls (seq (filter sdu/hl? %))]
                                      (mapv (partial inner (inc depth)) hls))))
                         hl->toc-el))
                   (repeatedly (constantly 0)) headlines)))
          (toc-els->toc
            ;; Wraps TOC elements into TOC node and returns it.
            [els]
            (when (seq els)
              (n/headline (cfg/toc-hl-val) (apply n/section els))))]

    (when-let [headlines (filter sdu/hl? children)]
      (-> headlines hls->toc-els toc-els->toc))))


;; root with head

(s/def :spacetools.spacedoc.org.head.root-with-head/children
  (s/alt :with-toc
         (s/cat
          :head ::n/section
          :toc ::toc
          :rest (s/* ::n/root-child))
         :no-toc
         (s/cat
          :head ::n/section
          :rest (s/* ::n/root-child))))

(s/def ::root-with-head
  (s/merge ::n/root
           (s/keys :req-un
                   [:spacetools.spacedoc.org.head.root-with-head/children])))


;; head with meta

(s/def :spacetools.spacedoc.org.head.with-meta/children
  (s/alt :with-tags
         (s/cat :title :spacetools.spacedoc.node.meta/title
                :tags :spacetools.spacedoc.node.meta/tags
                :rest (s/* ::n/block-element))
         :no-tags (s/cat :title :spacetools.spacedoc.node.meta/title
                         :rest (s/* ::n/block-element))))

(s/def ::with-meta
  (s/keys :req-un [:spacetools.spacedoc.node.section/tag
                   :spacetools.spacedoc.org.head.with-meta/children]))


;; root with meta

(s/def :spacetools.spacedoc.org.head.root-with-meta/children
  (s/alt :with-toc (s/cat :head ::with-meta
                          :toc ::toc
                          :rest (s/* ::n/root-child))
         :no-toc (s/cat :head ::with-meta
                        :rest (s/* ::n/root-child))))

(s/def ::root-with-meta
  (s/merge
   ::n/root
   (s/keys :req-un [:spacetools.spacedoc.org.head.root-with-meta/children])))


;;;; root node helpers

(defn-spec conj-toc ::n/root
  "Adds Table of content based on headlines present in the ROOT node"
  [{children :children :as root} ::n/root]
  (if-let [toc (root->toc root)]
    (let [[b-toc a-toc] (split-with (complement sdu/hl?) children)]
      (assoc root :children (vec (concat b-toc [toc] a-toc))))
    root))


(defn-spec str-tags->set
  :spacetools.spacedoc.node.val-spec/set-of-non-blank-strings
  "Given STR-TAGS string representation of ROOT tags return set of the tags."
  [str-tags string?]
  (into #{} (filter seq) (str/split str-tags #"\|")))


(defn-spec set-tags->str string?
  "Given SET-TAGS set of ROOT tags return string representation of the tags"
  [set-tags :spacetools.spacedoc.node.val-spec/set-of-non-blank-strings]
  (join "|" (sort set-tags)))


(defn-spec add-root-meta ::root-with-meta
  "Inline title and tags props of the ROOT node."
  [{:keys [title tags] :as root} ::n/root]
  (let [meta-vec (vec (list* (n/key-word "TITLE" title)
                             (when (seq tags)
                               [(n/key-word "TAGS" (set-tags->str tags))])))]
    (if (s/valid? ::root-with-head root)
      (update-in root [:children 0 :children] (partial into meta-vec))
      (update root :children (partial into [(apply n/section meta-vec)])))))


(s/def ::root-meta (s/or :title :spacetools.spacedoc.node.meta/title
                         :tags :spacetools.spacedoc.node.meta/tags))


(defn-spec remove-root-meta (s/nilable ::n/root)
  "Removes title and tags nodes from the ROOT node.
Returns nil if the node becomes empty."
  [root ::n/root]
  (if (s/valid? ::root-with-meta root)
    (sdu/remove-invalid
     (update-in root [:children 0 :children]
                (partial filterv #(not (s/valid? ::root-meta %)))))
    root))

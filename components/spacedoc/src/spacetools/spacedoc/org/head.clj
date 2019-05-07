(ns spacetools.spacedoc.org.head
  "Exporting to .org format."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join]]
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
(s/def :spacetools.spacedoc.org.head.toc.leaf/children
  (s/coll-of :spacetools.spacedoc.node/text
             :kind vector?
             :min-count 1
             :into []))
(s/def ::toc-leaf (s/keys :req-un
                          [:spacetools.spacedoc.org.head.toc.leaf/tag
                           :spacetools.spacedoc.org.head.toc.leaf/type
                           :spacetools.spacedoc.org.head.toc.leaf/path
                           :spacetools.spacedoc.org.head.toc.leaf/children]))

;; TOC item children wrapper (item-children)

(s/def :spacetools.spacedoc.org.head.toc.item-children/tag #{:item-children})
(s/def :spacetools.spacedoc.org.head.item-children/children
  (s/cat
   :headline-link ::toc-leaf
   :sub-headlines-links (s/?
                         (s/cat :line-break :spacetools.spacedoc.node/line-break
                                :brench (s/+ ::toc-branch)))))
(s/def ::toc-item-children
  (s/keys :req-un [:spacetools.spacedoc.org.head.toc.item-children/tag
                   :spacetools.spacedoc.org.head.item-children/children]))

;; TOC item (unordered list item)

(s/def :spacetools.spacedoc.org.head.toc.item/tag #{:list-item})
(s/def :spacetools.spacedoc.org.head.toc.item/type #{:unordered})
(s/def :spacetools.spacedoc.org.head.toc.item/bullet #{"- "})
(s/def :spacetools.spacedoc.org.head.toc.item/checkbox nil?)
(s/def :spacetools.spacedoc.org.head.toc.item/children
  (s/coll-of ::toc-item-children
             :kind vector?
             :min-count 1
             :into []))
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
  (s/coll-of ::toc-item
             :kind vector?
             :min-count 1
             :into []))
(s/def ::toc-branch
  (s/keys :req-un [:spacetools.spacedoc.org.head.toc.branch/tag
                   :spacetools.spacedoc.org.head.toc.branch/type
                   :spacetools.spacedoc.org.head.toc.branch/children]))


;; TOC items wrapper (section)

(s/def :spacetools.spacedoc.org.head.toc.wrapper/tag #{:section})
(s/def :spacetools.spacedoc.org.head.toc.wrapper/children
  (s/coll-of ::toc-branch
             :kind vector?
             :min-count 1
             :into []))
(s/def ::toc-wrapper
  (s/keys :req-un [:spacetools.spacedoc.org.head.toc.wrapper/tag
                   :spacetools.spacedoc.org.head.toc.wrapper/children]))

;; TOC (headline)

(s/def :spacetools.spacedoc.org.head.toc/tag #{:headline})
(s/def :spacetools.spacedoc.org.head.toc/todo? boolean?)
(s/def :spacetools.spacedoc.org.head.toc/children
  (s/coll-of ::toc-wrapper
             :kind vector?
             :into []))
(s/def :spacetools.spacedoc.org.head.toc/value
  #(= (cfg/toc-hl-val) %))
(s/def ::toc (s/keys :req-un [:spacetools.spacedoc.org.head.toc/tag
                              :spacetools.spacedoc.org.head.toc/todo?
                              :spacetools.spacedoc.org.head.toc/value
                              :spacetools.spacedoc.org.head.toc/children]))


(defn-spec root->toc (s/nilable ::toc)
  "Generate table of content for ROOT node.
Return nil if ROOT node doesn't have any headlines."
  [{children :children :as root} :spacetools.spacedoc.node/root]
  (letfn [(hl->gid-base [headlin] (sdu/hl-val->gh-id-base
                                   (sdu/fmt-hl-val (:value headlin))))

          (gh-id [gid-bs cnt] (if (> cnt 1) (str gid-bs "-" (dec cnt)) gid-bs))

          (up-*gid->count! [*gc hl]
            (let [gid-base (hl->gid-base hl)]
              ((vswap! *gc update gid-base (fnil inc 0)) gid-base)))

          (hl->toc-el [{:keys [toc-wrapper? value gh-id children]}]
            (if toc-wrapper?
              (some->> children
                       (apply n/section)
                       (n/headline (cfg/toc-hl-val)))
              (n/unordered-list
               (vec (list* (n/link gh-id (n/text (sdu/fmt-hl-val value)))
                           (when (seq children)
                             (list* (n/line-break) children)))))))

          (hls->toc [headlines]
            ;; NOTE: We use local `volatile!` because with the counter state
            ;;       threading the code gates much more complex.
            (let [*gid->count (volatile! {})]
              ((fn inner [depth hl]
                 (-> hl
                     (assoc :gh-id
                            (when-not (:toc-wrapper? hl)
                              (gh-id
                               (hl->gid-base hl)
                               (up-*gid->count! *gid->count hl))))
                     (update :children
                             #(when (< depth (cfg/toc-max-depth))
                                (when-let [hls (seq (filter sdu/hl? %))]
                                  (mapv (partial inner (inc depth)) hls))))
                     hl->toc-el))
               0 {:toc-wrapper? true :children (vec headlines)})))]

    (->> children
         (filter sdu/hl?)
         (hls->toc))))


;;;; root node head spec

;; root head

(s/def :spacetools.spacedoc.org.head.root-head/children
  (s/cat :title :spacetools.spacedoc.node.meta/title
         :tags (s/? :spacetools.spacedoc.node.meta/tags)
         :rest (s/* :spacetools.spacedoc.node/block-element)))

(s/def ::root-head
  (s/keys :req-un [:spacetools.spacedoc.node.section/tag
                   :spacetools.spacedoc.org.head.root-head/children]))


;; root with head

(s/def :spacetools.spacedoc.org.head.root-with-head-props/children
  (s/cat
   :head ::root-head
   :toc (s/? ::toc)
   :rest (s/* :spacetools.spacedoc.node/root-child)))

(s/def ::root-with-head-props
  (s/keys :req-un [:spacetools.spacedoc.node.root/title
                   :spacetools.spacedoc.node.root/tags
                   :spacetools.spacedoc.org.head.root-with-head-props/children]
          :opt-un [:spacetools.spacedoc.node.root/source
                   :spacetools.spacedoc.node.root/root-dir]))


;;;; root node helpers

(defn-spec conj-toc :spacetools.spacedoc.node/root
  "Add Table of content based on headlines present in the ROOT node"
  [{children :children :as root} :spacetools.spacedoc.node/root]
  (if-let [toc (root->toc root)]
    (let [[b-toc a-toc] (split-with (complement sdu/hl?) children)]
      (assoc root :children (vec (concat b-toc [toc] a-toc))))
    root))


(defn-spec inline-head-props ::root-with-head-props
  "Add title and tags nodes to the head of the root node."
  [{tags :tags title :title [f-child & children] :children :as root}
   :spacetools.spacedoc.node/root]
  (let [title-n (n/key-word "TITLE" title)
        tags-n (when (seq tags) (n/key-word "TAGS" (join "|" (sort tags))))
        head-childen (when (s/valid? :spacetools.spacedoc.node/section f-child)
                       (:children f-child))]
    (update root :children
            #(apply vector
                    (->> head-childen
                         (list* title-n tags-n)
                         (remove nil?)
                         (apply n/section))
                    (if head-childen children %)))))


(s/def :spacetools.spacedoc.org.head/root-head-prop
  (s/or :title :spacetools.spacedoc.node.meta/title
        :tags :spacetools.spacedoc.node.meta/tags))


(defn-spec remove-head-props :spacetools.spacedoc.node/root
  "Remove title and tags nodes from the head of the root node."
  [{[f-child & children] :children :as root} :spacetools.spacedoc.node/root]
  (if (s/valid? :spacetools.spacedoc.node/section f-child)
    (update-in
     root
     [:children 0 :children]
     (partial into [] (->> :spacetools.spacedoc.org.head/root-head-prop
                           (partial s/valid?)
                           (remove))))
    root))

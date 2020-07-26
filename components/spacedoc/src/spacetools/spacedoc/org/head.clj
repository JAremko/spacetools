(ns spacetools.spacedoc.org.head
  "Specs and helpers for working with headers of documents."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join]]
            [clojure.spec.gen.alpha :as gen]
            [orchestra.core :refer [defn-spec]]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu]
            [clojure.string :as str]))


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
             :max-count 1
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
   :sub-headlines-links
   (s/? (s/cat :line-break :spacetools.spacedoc.node/line-break
               ;; TODO:  It's a recursive spec and `s/*recursion-limit*` seems
               ;;        to be ignored.
               ;;        see https://clojure.atlassian.net/browse/CLJ-1978
               ;;        this needs a solution.
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
  (s/merge :spacetools.spacedoc.node/root
           (s/keys
            :req-un
            [:spacetools.spacedoc.org.head.root-with-head-props/children])))


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


(defn-spec remove-inline-head-props :spacetools.spacedoc.node/root
  "Removes head props children nodes from the ROOT node."
  [root :spacetools.spacedoc.node/root]
  (if (s/valid? ::root-with-head-props root)
    (assoc root :children
           #(let [{{{title :title tags :tags head-rest :rest :as head} :head
                    toc :toc
                    root-rest :rest}
                   :children :as children}
                  (s/conform ::root-with-head-props root)]
              (if head-rest
                (into [(apply n/section head-rest)] (rest children))
                children)))
    root))

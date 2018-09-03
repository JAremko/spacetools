(ns ^{:doc "Exporting SDN to .org format."}
    spacedoc.data.org
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [split-lines join]]
            [spacedoc.data :as data]
            [spacedoc.data.node :as n]))



(s/def :spacedoc.data.node.baz/value (s/coll-of any?
                                                :kind vector?
                                                :min-count 1
                                                :into []))


(n/defnode ::baz (s/keys :req-un [:spacedoc.data.node.baz/value]))

(baz ['s])


(def ^:private emphasis-tokens {:bold "*"
                                :italic "/"
                                :verbatim "="
                                :underline "_"
                                :kbd "~"  ;; Called code in "the classic ORG".
                                :strike-through "+"})


(def ^:private block-container-delims {:verse ["#+BEGIN_VERSE\n"
                                               "#+END_VERSE\n"]
                                       :quote ["#+BEGIN_QUOTE\n"
                                               "#+END_QUOTE\n"]
                                       :center ["#+BEGIN_CENTER\n"
                                                "#+END_CENTER\n"]
                                       :section ["" ""]})


(def ^:private list-identation 2)


(def
  ^{:doc "These nodes can be converted only in their parent context."
    :private true}
  indirect-nodes
  #{:headline :item-children :item-tag :table-row :table-cell})


(def ^:private kinds {n/inline-container-tags :inline-container
                      n/inline-leaf-tags :inline-leaf
                      n/block-tags :block
                      n/headline-tags :headline})


(defn- tag->kind
  [tag]
  (some->> kinds
           (filter #((key %) tag))
           (first)
           (val)))


(defmulti sdn->org
  (fn [{tag :tag :as node}]
    {:pre  [(complement (indirect-nodes tag))
            (map? node)
            (keyword? tag)]}
    (cond
      ;; Headline node group.
      (n/headline-tags tag) :headline

      ;; List node group.
      (#{:feature-list :plain-list} tag) :list

      ;; Emphasis containers.
      (#{:bold :italic :underline :strike-through} tag) :emphasis-container

      ;; Block-container node group.
      ((set (keys block-container-delims)) tag) :block-container

      ;; Everything else.
      :else tag)))


;;;; Helpers

(defn- nl-wrap?
  [node-tag]
  {:pre [(keyword? node-tag)]}
  (and
   (not (#{:plain-list :feature-list} node-tag))
   (#{:block :headline} (tag->kind node-tag))))


(defn- nl-after?
  [node-tag]
  {:pre [(keyword? node-tag)]}
  (#{:block :headline} (tag->kind node-tag)))


(defn- conv*
  [node-seq]
  {:pre [((some-fn vector? nil?) node-seq)]}
  (reduce (fn [acc next]
            (let [h-t (:head-tag (meta acc))
                  b-s (last acc)
                  n-s (sdn->org next)]
              (with-meta
                (conj acc
                      (str (cond
                             (not (and b-s n-s)) ""
                             (or (nl-after? h-t) (nl-wrap? (:tag next))) "\n"
                             (not (or (data/seps (last b-s))
                                      (data/seps (first n-s)))) " "
                             :else "")
                           n-s))
                {:head-tag (:tag next)})))
          []
          node-seq))


(defn- conv
  [node-seq]
  {:pre [((some-fn vector? nil?) node-seq)]}
  (join (conv* node-seq)))


;;;; Groups of nodes (many to one).


(defmethod sdn->org :emphasis-container
  [{:keys [tag children]}]
  (let [token (emphasis-tokens tag)]
    (str token (conv children) token)))


(defmethod sdn->org :list
  [{children :children}]
  (conv children))


(defmethod sdn->org :block-container
  [{:keys [tag children]}]
  (let [{[begin-token end-token] tag} block-container-delims]
    (str begin-token (conv children) end-token)))


;;;; Individual nodes (one to one).


(defmethod sdn->org :paragraph
  [{children :children}]
  (conv children))


(defn- table->vec-rep
  [{rows :children}]
  {:pre [((some-fn vector? nil?) rows)]}
  (let [vec-tab (mapv
                 (fn [{type :type cells :children}]
                   (if (= type :standard)
                     (mapv #(str " " (conv (:children %)) " ") cells)
                     []))
                 rows)
        cols-w (apply mapv
                      (fn [& cols]
                        (apply max (map count cols)))
                      (remove empty? vec-tab))]
    (vec (concat [cols-w] vec-tab))))


(defn- make-table-rule-str
  [cols-w]
  {:pre [(s/valid? (s/coll-of pos-int?) cols-w)]}
  (join "+" (map #(join (repeat % "-")) cols-w)))


(defn- make-table-row-str
  [row cols-w]
  {:pre [(vector? row)
         (s/valid? (s/coll-of pos-int?) cols-w)]}
  (join "|"
        (map (fn [column-width cell-s]
               (str cell-s
                    (join (repeat (- column-width
                                     (count cell-s))
                                  " "))))
             cols-w
             row)))


(defmethod sdn->org :table
  [table]
  (let [[cols-w & vrep] (table->vec-rep table)]
    (str "\n"
         (join "\n"
               (map (fn [row]
                      (str "|"
                           (if (empty? row)
                             (make-table-rule-str cols-w)
                             (make-table-row-str row cols-w))
                           "|"))
                    vrep))
         "\n")))


(defmethod sdn->org :table-cell
  [{children :children}]
  (conv children))


(defmethod sdn->org :table-row
  [{:keys [type children]}]
  (if (= type :standard)
    (str "| " (join " |" (conv* children)) " |")
    "|-"))


(defmethod sdn->org :link
  [{:keys [raw-link children]}]
  (format "[[%s]%s]"
          raw-link
          (if (seq children)
            (format "[%s]" (conv children))
            "")))


(defmethod sdn->org :list-item
  [{b :bullet c :checkbox [{children :children} item-tag] :children}]
  (let [itag (some->> item-tag
                      (:children)
                      (conv))]
    (str (apply str
                b
                (if itag (format "%s :: " itag) "")
                (->> children
                     (conv)
                     (split-lines)
                     (remove empty?)
                     (join (apply str "\n" (repeat list-identation " ")))))
         "\n")))


(defmethod sdn->org :example
  [{value :value}]
  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n" value))


(defmethod sdn->org :src
  [{:keys [language value]}]
  (format "#+BEGIN_SRC %s\n%s#+END_SRC\n" language value))


(defmethod sdn->org :text
  [{value :value}]
  value)


(defmethod sdn->org :superscript
  [{children :children}]
  (apply str "^" (conv* children)))


(defmethod sdn->org :subscript
  [{children :children}]
  (apply str "_" (conv* children)))


(defmethod sdn->org :line-break
  [_]
  "\n")


(defmethod sdn->org :key-word
  [{:keys [key value]}]
  (format "#+%s: %s\n" key value))


(defmethod sdn->org :headline
  [{value :value lvl :level children :children :as hl}]
  (str
   (join (repeat lvl "*"))
   " "
   value
   "\n"
   (conv (mapv #(if (n/headline-tags (:tag %)) (data/fill-hl hl %) %)
               children))))


(defmethod sdn->org :verbatim
  [{:keys [tag value]}]
  (let [token (emphasis-tokens tag)]
    (str token value token)))


(defmethod sdn->org :kbd
  [{:keys [tag value]}]
  (let [token (emphasis-tokens tag)]
    (str token (join " " value) token)))


(defmethod sdn->org :root
  [{children :children}]
  (conv (mapv #(if (n/headline-tags (:tag %)) (data/fill-hl %) %) children)))

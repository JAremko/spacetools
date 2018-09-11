(ns spacedoc.data.org
  "Exporting SDN to .org format."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :refer [join]]
            [clojure.string :as str]
            [spacedoc.data :as data]
            [spacedoc.data.node :as n]))


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


(def ^:private list-indentation 2)


(def ^:private beging-end-indentation 2)


(def ^:private table-indentation 0)


(def indirect-nodes
  "These nodes can be converted only in their parent context."
  #{:headline :item-children :item-tag :table-row :table-cell})


(def ^:private kinds {n/inline-container-tags :inline-container
                      n/inline-leaf-tags :inline-leaf
                      n/block-tags :block
                      n/headline-tags :headline})


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

(defn- indent-str
  [indent-level s]
  (if (str/blank? s)
    s
    (let [ind (apply str (repeat indent-level " "))]
      (->> (str/split-lines s)
           (map #(str ind % "\n"))
           (reduce #(str %1 (if (empty? %2) "\n" %2)) "")))))


(defn- length
  "Like `count` but for strings only and accounts for zero-width spaces."
  [^String s]
  (count (str/replace s "\u200B" "")))


(defn- tag->kind
  [tag]
  (some->> kinds
           (filter #((key %) tag))
           (first)
           (val)))


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


(defn- nl-between?
  [first-node-tag second-node-tag]
  {:pre [(every? keyword? [first-node-tag second-node-tag])]}
  (= :paragraph first-node-tag second-node-tag))


(defn- conv*
  [node-seq]
  {:pre [((some-fn vector? nil?) node-seq)]}
  (reduce (fn [acc next]
            (let [h-t (:head-tag (meta acc))
                  b-s (last acc)
                  n-s (sdn->org next)]
              (with-meta
                (conj acc
                      ;; Figuring out how to separate children
                      (str (cond
                             (not (and b-s n-s)) ""
                             (nl-between? h-t (:tag next)) "\n"
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
  (str begin-token
       ;; NOTE: We don't "hard-code" indentation into sections
       (indent-str (if (= tag :section) 0 beging-end-indentation)
                   (conv children))
       end-token)))


;;;; Individual nodes (one to one).


(defmethod sdn->org :paragraph
[{children :children}]
(conv children))


(defn table->vec-rep
[{rows :children}]
{:pre [((some-fn vector? nil?) rows)]}
(let [vec-tab (mapv
               (fn [{type :type cells :children}]
                 (if (= type :standard)
                   (mapv #(str " " (conv (:children %)) " ") cells)
                   []))
               rows)
      cols-w (if-let [ne-vec-tab (seq (remove empty? vec-tab))]
               (apply mapv
                      (fn [& cols]
                        (apply max (map length cols)))
                      ne-vec-tab)
               [])]
  (vec (concat [cols-w] vec-tab))))


(defn- table-rule-str
  [cols-w]
  {:pre [(s/valid? (s/coll-of pos-int?) cols-w)]}
  (join "+" (map #(join (repeat % "-")) cols-w)))


(defn- table-row-str
  [row cols-w]
  {:pre [(vector? row)
         (s/valid? (s/coll-of pos-int?) cols-w)]}
  (join "|"
        (map (fn [column-width cell-s]
               (str cell-s
                    (join (repeat (- column-width
                                     (length cell-s))
                                  " "))))
             cols-w
             row)))


(defmethod sdn->org :table
  [table]
  (let [[cols-w & vrep] (table->vec-rep table)]
    (str "\n"
         (indent-str table-indentation
                     (join "\n"
                           (map (fn [row]
                                  (str "|"
                                       (if (seq cols-w)
                                         (if (empty? row)
                                           (table-rule-str cols-w)
                                           (table-row-str row cols-w))
                                         "")
                                       "|"))
                                vrep)))
         "\n")))


(defn- fmt-cell-content
  "FIXME: Tables shouldn't have newlines or pipes
  but silently removing them is sub-optimal."
  [t-c-children-str]
  (str/replace t-c-children-str #"\n|\|" " "))


(defmethod sdn->org :table-cell
  [{children :children}]
  (fmt-cell-content (conv children)))


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
                (str/trim (indent-str list-indentation (conv children))))
         "\n")))


(defmethod sdn->org :example
  [{value :value}]
  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n"
          (indent-str beging-end-indentation value)))


(defmethod sdn->org :src
  [{:keys [language value]}]
  (format "#+BEGIN_SRC %s\n%s#+END_SRC\n"
          language
          (indent-str beging-end-indentation value)))


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
   (join (repeat (or lvl 1) "*"))
   " "
   value
   "\n"
   (conv (mapv #(if (n/headline-tags (:tag %)) (data/fill-hl hl %) %)
               children))))
(conv nil)

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

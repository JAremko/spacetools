(ns spacedoc.data.org
  (:require [clojure.string :refer [split-lines join]]
            [clojure.set :refer [intersection]]
            [spacedoc.data :refer [seps headline-tags tag->kind fill-hl]]))


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
  ^{:doc "These nodes can be converted only in their parent context."}
  indirect-nodes
  #{:headline :item-children :item-tag :table-row :table-cell})


(defmulti sdn->org
  (fn [{tag :tag}]
    {:pre  [(complement (indirect-nodes tag))]}
    (cond
      ;; Headline node group.
      (headline-tags tag) :headline

      ;; List node group.
      (#{:feature-list :plain-list} tag) :list

      ;; Emphasis node group.
      ((set (keys emphasis-tokens)) tag) :emphasis

      ;; Block-container node group.
      ((set (keys block-container-delims)) tag) :block-container

      ;; Everything else.
      :else tag)))


;;;; Helpers


(defn- nl-wrap?
  [node-tag]
  (and
   (not (#{:plain-list :feature-list} node-tag))
   (#{:block :headline} (tag->kind node-tag))))


(defn- nl-after?
  [node-tag]
  (#{:block :headline} (tag->kind node-tag)))


(def ^:private conv*
  (partial reduce
     (fn [acc next]
       (let [h-t (:head-tag (meta acc))
             b-s (last acc)
             n-s (sdn->org next)]
         (with-meta
           (conj acc
                 (str (cond
                        (not (and b-s n-s)) ""
                        (or (nl-after? h-t) (nl-wrap? (:tag next))) "\n"
                        (not (or (seps (last b-s)) (seps (first n-s)))) " "
                        :else "")
                      n-s))
           {:head-tag (:tag next)})))
     []))


(defn- conv
  [node-seq]
  (join (conv* node-seq)))


;;;; Groups of nodes (many to one).


(defmethod sdn->org :list
  [{children :children}]
  (conv children))


(defmethod sdn->org :emphasis
  [{:keys [tag value children]}]
  (let [token (emphasis-tokens tag)]
    (str token (or (join value) (conv children)) token)))


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
  (join "+" (map #(join (repeat % "-")) cols-w)))


(defn- make-table-row-str
  [row cols-w]
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
  (let [itag (cond
               (string? item-tag) (item-tag)
               (map? item-tag) (sdn->org (:value item-tag))
               :else nil)]
    (str
     (apply
      str
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


(defmethod sdn->org :plain-text
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
   (apply str (repeat lvl "*"))
   " "
   value
   "\n"
   (conv (mapv #(if (headline-tags (:tag %)) (fill-hl hl %) %) children))))


(defmethod sdn->org :root
  [{children :children}]
  (conv (mapv #(if (headline-tags (:tag %)) (fill-hl %) %) children)))

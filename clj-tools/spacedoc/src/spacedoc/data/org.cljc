(ns spacedoc.data.org
  (:require [clojure.string :refer [split-lines join]]
            [clojure.set :refer [intersection]]
            [spacedoc.data :refer [seps headline-tags tag->kind]]
            [spacedoc.data.nim :refer [nim-body]]))


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


(defmulti sdn->org
  (fn [{tag :tag}]
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
      :else (case tag
              :headline
              (throw
               (Exception.
                "Meta headline must be converted into concrete node"))
              (:item-children :item-tag)
              (throw
               (Exception.
                (format "\"%s\" node can't be converted directly" (name tag))))
              tag))))


;;;; Helpers


(def ^:private conv*
  (partial reduce
     (fn [acc next]
       (let [head-kind (tag->kind (:head-tag (meta acc)))
             before-str (last acc)
             next-str (sdn->org next)
             next-kind (tag->kind (:tag next))
             edge? (not (and before-str next-str))
             separated? (or (seps (last before-str))
                            (seps (first next-str)))]
         (println "!!! " (:head-tag (meta acc)) " " (:tag next) " " (intersection
                                                                     #{:block :headline}
                                                                     (hash-set head-kind next-kind)) " !!!!")
         (with-meta
           (conj acc
                 (str (cond (seq
                             (intersection
                              #{:block :headline}
                              (hash-set head-kind next-kind)))
                            "\n"
                            (not (or edge? separated?))
                            " "
                            :else "")
                      next-str))
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


(defmethod sdn->org :table
  [{rows :children}]
  (str (join "\n" (map sdn->org rows)) "\n"))


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


(defmethod sdn->org :keyword
  [{:keys [key value]}]
  (format "#+%s: %s\n" key value))


(defmethod sdn->org :headline
  [{:keys [value level children]}]
  (str
   (apply str (repeat level "*"))
   " "
   value
   "\n"
   (conv children)))


(defmethod sdn->org :root
  [{[body] :children}]
  (sdn->org body))


(defmethod sdn->org :body
  [{children :children}]
  (conv children))


(spit
 "/mnt/workspace/test/spacedoc/clj-tools/spacedoc/src/spacedoc/data/test.org"
 (sdn->org nim-body))

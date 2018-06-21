(ns spacedoc.data.org
  (:require [spacedoc.data.helpers :as h]
            [clojure.string :refer [split-lines join]]
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


(defmulti ^:private sdn-node->org-string
  (fn [{tag :tag}]
    (cond
      ;; Headline node group.
      (or (#{:description :todo} tag)
          (re-matches #"^headline-level-.*$" (name tag))) :headline

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


(def ^:private conv-all (partial mapv sdn-node->org-string))


(defn- nodes->str
  [node-seq]
  (apply str (conv-all node-seq)))


;;;; Groups of nodes (many to one).


(defmethod sdn-node->org-string :list
  [{children :children}]
  (str (nodes->str children) "\n"))


(defmethod sdn-node->org-string :emphasis
[{:keys [tag value children]}]
(let [token (emphasis-tokens tag)]
  (str token (apply str (or value (conv-all children))) token " ")))


(defmethod sdn-node->org-string :block-container
  [{:keys [tag children]}]
  (let [{[begin-token end-token] tag} block-container-delims]
    (str begin-token (nodes->str children) end-token)))


;;;; Individual nodes (one to one).


(defmethod sdn-node->org-string :table
  [{rows :children}]
  (str
   "\n\n"
   (join "\n" (conv-all rows))
   "\n"))


(defmethod sdn-node->org-string :table-cell
  [{children :children}]
  (nodes->str children))


(defmethod sdn-node->org-string :table-row
  [{:keys [type children]}]
  (if (= type :standard)
    (str "| " (join " | " (conv-all children)) " |")
    "|-"))


(defmethod sdn-node->org-string :link
  [{:keys [raw-link children]}]
  (format "[[%s]%s]"
          raw-link
          (if (seq children)
            (format "[%s]" (nodes->str children))
            "")))


(defmethod sdn-node->org-string :list-item
  [{b :bullet c :checkbox [{children :children} item-tag] :children}]
  (let [itag (cond
               (string? item-tag) (item-tag)
               (map? item-tag) (sdn-node->org-string (:value item-tag))
               :else nil)]
    (apply
     str
     "\n"
     b
     (if itag (format "%s :: " itag) "")
     (->> children
          (nodes->str)
          (split-lines)
          (remove empty?)
          (join "\n")))))


(defmethod sdn-node->org-string :example
  [{value :value}]
  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n" value))


(defmethod sdn-node->org-string :src
  [{:keys [language value]}]
  (format "#+BEGIN_SRC %s\n%s#+END_SRC\n" language value))


(defmethod sdn-node->org-string :plain-text
  [{value :value}]
  value)


(defmethod sdn-node->org-string :superscript
  [{children :children}]
  (apply str "^" (conv-all children)))


(defmethod sdn-node->org-string :subscript
  [{children :children}]
  (apply str "_" (conv-all children)))


(defmethod sdn-node->org-string :line-break
  [_]
  "\n")


(defmethod sdn-node->org-string :keyword
  [{:keys [key value]}]
  (format "#+%s: %s\n" key value))


(defmethod sdn-node->org-string :paragraph
  [{children :children}]
  (format "\n%s\n" (nodes->str children)))


(defmethod sdn-node->org-string :headline
  [{:keys [value level children]}]
  (apply str
         "\n"
         (apply str (repeat level "*"))
         " "
         value
         (conv-all children)))


(defmethod sdn-node->org-string :root
  [{[body] :children}]
  (sdn-node->org-string body))


(defmethod sdn-node->org-string :body
  [{children :children}]
  (nodes->str children))


(defn- remove-spaces-before-seps
  [org-string]
  org-string)


(defn orgify
  [sdn]
  (->> sdn
       (remove-spaces-before-seps)
       (sdn-node->org-string)))


(spit "/mnt/workspace/test/spacedoc/clj-tools/spacedoc/src/spacedoc/data/test.org" (orgify nim-body))

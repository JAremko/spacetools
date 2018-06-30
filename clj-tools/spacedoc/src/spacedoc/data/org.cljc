(ns spacedoc.data.org
  (:require [clojure.string :refer [split-lines join]]
            [spacedoc.data :as data]
            [spacedoc.data.nim :refer [nim-body]]))


;; FIXME: We cut some corners here.. For example, we export tables without
;;        properly aligning them, producing stacks of empty lines. Then we use
;;        spacefmt (format from "emacs-tools" dir) to fix it. This way
;;        we can reuse code from the tool and org-mode package itself.
;;        But it will be much clearer (and correct) if each element will be
;;        responsible only for rendering it's children and not how to pad
;;        itself (when needed) with spaces, empty-lines etc. in the parent
;;        container.


(def ^:private emphasis-tokens {:bold "*"
                                :italic "/"
                                :verbatim "="
                                :underline "_"
                                :kbd "~"  ;; Called code in "the classic ORG".
                                :strike-through "+"})


(def ^:private block-container-delims {:verse ["#+BEGIN_VERSE\n"
                                               "#+END_VERSE"]
                                       :quote ["#+BEGIN_QUOTE\n"
                                               "#+END_QUOTE"]
                                       :center ["#+BEGIN_CENTER\n"
                                                "#+END_CENTER"]
                                       :section ["" ""]})


(def list-identation 2)


(defmulti sdn->org
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


;;;; Helpers

(def ^:private seps  #{\! \? \: \; \) \, \. \- \\ \newline \space \tab})


(def ^:private conv-all (partial mapv sdn->org))


(defn- conv
  [node-seq]
  (join (conv-all node-seq)))


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
  (join "\n" (conv-all rows)))


(defmethod sdn->org :table-cell
  [{children :children}]
  (conv children))


(defmethod sdn->org :table-row
  [{:keys [type children]}]
  (if (= type :standard)
    (str "| " (join " | " (conv-all children)) " |")
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
  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE" value))


(defmethod sdn->org :src
  [{:keys [language value]}]
  (format "#+BEGIN_SRC %s\n%s#+END_SRC" language value))


(defmethod sdn->org :plain-text
  [{value :value}]
  value)


(defmethod sdn->org :superscript
  [{children :children}]
  (apply str "^" (conv-all children)))


(defmethod sdn->org :subscript
  [{children :children}]
  (apply str "_" (conv-all children)))


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

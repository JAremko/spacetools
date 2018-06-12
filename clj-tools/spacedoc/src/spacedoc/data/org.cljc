(ns spacedoc.data.org
  (:require [spacedoc.util :as util]

            [spacedoc.data.nim :refer [nim-body]]

            [clojure.core.reducers :as r]
            [clojure.set :refer [union]]
            [clojure.string :refer [join]]
            [clojure.spec.alpha :as s]))


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
                                                "#+BEGIN_CENTER\n"]
                                       :section ["" ""]})


(defmulti ^:private sdn-node->org-string
  (fn [{tag :tag}]
    (cond

      ;; Headline node family.
      (or (#{:description :todo} tag)
          (re-matches #"^headline-level-.*$" (name tag))) :headline

      ;; List node family.
      (#{:feature-list :plain-list} tag) :list

      ;; Emphasis node family.
      ((set (keys emphasis-tokens)) tag) :emphasis

      ;; Block-container node family.
      ((set (keys block-container-delims)) tag) :block-container

      ;; Everything else.
      :else (case tag
              :headline
              (throw
               (Exception.
                "Meta headline must be converted into concrete node"))
              (:list-item :item-children :item-tag :table-row :table-cell)
              (throw
               (Exception.
                (format "\"%s\" node can't be converted directly" (name tag))))
              tag))))


(def ^:private conv-all (partial mapv sdn-node->org-string))


(defmethod sdn-node->org-string :emphasis
  [{:keys [tag value children]}]
  (let [token (emphasis-tokens tag)]
    (str token (apply str (or value (conv-all children))) token)))


(defmethod sdn-node->org-string :superscript
  [{children :children}]
  (apply str "^" (conv-all children)))


(defmethod sdn-node->org-string :line-break
  [_]
  "\n")


(defmethod sdn-node->org-string :block-container
  [{:keys [tag children]}]
  (let [{[begin-token end-token] tag} block-container-delims]
    (str begin-token (apply str (conv-all children)) end-token)))


(defmethod sdn-node->org-string :org-file-path
  [{:keys [tag value children]}]

  )


(defmethod sdn-node->org-string :src
  [{:keys [tag language value]}]

  )


(defmethod sdn-node->org-string :embeddable-web-link
  [{:keys [tag value children]}]

  )


(defmethod sdn-node->org-string :subscript
  [{:keys [tag children]}]

  )


(defmethod sdn-node->org-string :plain-text
  [{:keys [tag value]}]

  )


(defmethod sdn-node->org-string :keyword
  [{:keys [key value]}]
  (format "#+%s: %s\n" key value))


(defmethod sdn-node->org-string :embeddable-file-path
  [{:keys [tag value children]}]

  )


(defmethod sdn-node->org-string :fixed-width
  [{:keys [tag value]}]

  )


(defmethod sdn-node->org-string :file-path
  [{:keys [tag value children]}]

  )


(defmethod sdn-node->org-string :example
  [{:keys [tag value]}]

  )


(defmethod sdn-node->org-string :paragraph
  [{children :children}]
  (format "%s\n" (apply str (conv-all children))))


(defmethod sdn-node->org-string :web-link
  [{:keys [tag value children]}]

  )


(defmethod sdn-node->org-string :local-org-web-link
  [{:keys [tag value raw-link target-headline-gh-id children]}])


(defmethod sdn-node->org-string :headline-link
  [{:keys [tag target-headline-gh-id children]}]

  )


(defmethod sdn-node->org-string :table
  [{:keys [tag children]}])


(defmethod sdn-node->org-string :headline
  [{:keys [value level children]}]
  (format "%s\n"
          (apply str
                 (format "%s %s\n" (apply str (repeat level "*")) value)
                 (conv-all children))))


(defmethod sdn-node->org-string :list
  [{:keys [tag type children]}])


(defmethod sdn-node->org-string :root
  [{[body] :children}]
  (sdn-node->org-string body))


(defmethod sdn-node->org-string :body
  [{children :children}]
  (apply str (conv-all children)))


(defn orgify
  [sdn]
  (sdn-node->org-string sdn))


(spit "/mnt/workspace/test/spacedoc/clj-tools/spacedoc/src/spacedoc/data/test.org" (orgify nim-body))

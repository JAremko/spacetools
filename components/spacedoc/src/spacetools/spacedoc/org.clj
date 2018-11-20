(ns spacetools.spacedoc.org
  "Exporting SDN to .org format."
  (:require [clojure.core.match :refer [match]]
            [clojure.core.reducers :as r]
            [clojure.set :sa :set]
            [clojure.spec.alpha :as s]
            [clojure.string :refer [join]]
            [clojure.string :as str]
            [spacetools.spacedoc-util.interface :as sdu]
            [spacetools.spacedoc.node :as n]))


(def emphasis-tokens {:bold "*"
                      :italic "/"
                      :verbatim "="
                      :underline "_"
                      :kbd "~"  ;; Called code in "the classic ORG".
                      :strike-through "+"})

(def block-container-delims {:verse ["#+BEGIN_VERSE\n"
                                     "#+END_VERSE\n"]
                             :quote ["#+BEGIN_QUOTE\n"
                                     "#+END_QUOTE\n"]
                             :center ["#+BEGIN_CENTER\n"
                                      "#+END_CENTER\n"]
                             :section ["" ""]})

(def begin-end-indentation 2)

(def table-indentation 0)

(def indirect-nodes
  "These nodes can be converted only in their parent context."
  #{:item-children :item-tag :table-row :table-cell})

(def kinds {n/inline-container-tags :inline-container
            n/inline-leaf-tags :inline-leaf
            n/block-tags :block
            n/headlines-tags :headline})


(defmulti sdn->org
  (fn [{tag :tag :as node}]
    {:pre  [((complement indirect-nodes) tag)
            (map? node)
            (keyword? tag)]}
    (cond
      ;; Headline node group.
      (n/headlines-tags tag) :headline

      ;; List node group.
      (#{:feature-list :plain-list} tag) :list

      ;; Emphasis containers.
      (#{:bold :italic :underline :strike-through} tag) :emphasis-container

      ;; Block-container node group.
      ((set (keys block-container-delims)) tag) :block-container

      ;; Everything else.
      :else tag)))


;;;; Helpers

(defn assoc-toc
  [{children :children :as root}]
  {:pre [(s/valid? :spacetools.spacedoc.node/root root)]
   :post [(s/valid? :spacetools.spacedoc.node/root %)]}

  (letfn [(hl? [node] (n/headlines-tags (:tag node)))

          (hl->gid-base [headlin] (sdu/hl-val->gh-id-base
                                   (sdu/fmt-hl-val (:value headlin))))

          (gh-id [gid-bs cnt] (if (> cnt 1) (str gid-bs "-" (dec cnt)) gid-bs))

          (up-*gid->count! [*gc hl]
            (let [gid-base (hl->gid-base hl)]
              ((vswap! *gc update gid-base #(inc (or % 0)))
               gid-base)))

          (hl->toc-el [{:keys [toc-wrapper? value gh-id children]}]
            (if toc-wrapper?
              (some->> children
                       (apply n/section)
                       (n/headline sdu/toc-hl-val))
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
                             #(when (< depth sdu/toc-max-depth)
                                (some->>
                                 %
                                 (filter hl?)
                                 (seq)
                                 (mapv (partial inner (inc depth))))))
                     hl->toc-el))
               0
               {:toc-wrapper? true :children (vec headlines)})))]

    (if-let [toc (some->> children (filter hl?) (hls->toc))]
      (let [[b-toc a-toc] (split-with (complement hl?) children)]
        (assoc root :children (vec (concat b-toc [toc] a-toc))))
      root)))


(defn viz-len
  "Like `count` but returns real visual length of a string."
  [^String s]
  (-> s
      (str/replace #"\[(?:\[[^\[\]]+\]){0,1}\[([^\[\]]+)\]+\]" "$1")
      (str/replace "\u200B" "")
      (count)))


(defn tag->kind
  [tag]
  (some->> kinds
           (filter #((key %) tag))
           (first)
           (val)))


(defn sep-inlines
  [t1 s1 t2 s2]
  (when (and (every? not-empty [s1 s2])
             (not (= :text t1 t2)))
    (let [l-s1-sep? (sdu/seps-right (last s1))
          f-s2-sep? (sdu/seps-left (first s2))]
      (when (not (or l-s1-sep? f-s2-sep?)) " "))))


(defn sep-blocks
  [p-t t1 t2]
  (let [[t1-k t2-k] (mapv tag->kind [t1 t2])]
    (match [p-t            t1          t1-k        t2            t2-k     ]
           [:item-children :paragraph  _           ::end         _        ] ""
           [:item-children :plain-list _           ::end         _        ] ""
           [:item-children _           :block      ::end         _        ] "\n"
           [_              _           _           ::end         _        ] ""
           [:section       nil         _           :table        _        ] "\n"
           [_              nil         _           _             _        ] ""
           [_              _           _           :plain-list   _        ] ""
           [_              _           _           :feature-list _        ] ""
           [_              _           :headline   _             _        ] "\n"
           [_              _           _           _             :headline] "\n"
           [_              _           :block      _             _        ] "\n"
           [_              _           _           _             :block   ] "\n"
           :else nil)))


(defn conv*
  "Reduce CHILDREN vector into vector of ORG string representations inserting
  proper separators (new lies and white spaces).
  P-TAG is parent node tag."
  [p-tag children]
  {:pre [((some-fn vector? nil?) children)]}
  (r/reduce (r/monoid
             (fn [acc [n-t n-s]]
               (let [{h-t :head-tag h :head} (meta acc)
                     next (str (or (sep-inlines h-t h n-t n-s)
                                   (sep-blocks p-tag h-t n-t)
                                   "")
                               n-s)]
                 (with-meta (conj acc next) {:head next :head-tag n-t})))
             vector)
            (r/map #(vector (:tag %) (sdn->org %))
                   (conj children {:tag ::end}))))


(defn conv
  "Reduce CHILDREN vector into `str/join`ed ORG string using `conv*`
  P-TAG is parent node tag."
  [p-tag children]
  {:pre [((some-fn vector? nil?) children)]}
  (join (conv* p-tag children)))


;;;; Groups of nodes (many to one).

(defmethod sdn->org :emphasis-container
  [{:keys [tag children]}]
  (let [token (emphasis-tokens tag)]
    (str token (conv tag children) token)))


(defmethod sdn->org :list
  [{:keys [tag children]}]
  (str (str/trim-newline (conv tag children)) "\n"))


(defmethod sdn->org :block-container
  [{:keys [tag children]}]
  (let [{[begin-token end-token] tag} block-container-delims]
    (str begin-token
         ;; NOTE: We don't "hard-code" indentation into sections
         (sdu/indent (if (= tag :section) 0 begin-end-indentation)
                 (conv tag children))
         end-token)))


;;;; Individual nodes (one to one).

(defmethod sdn->org :paragraph
  [{:keys [tag children]}]
  (conv tag children))


(defn table->vec-rep
  [{rows :children}]
  {:pre [((some-fn vector? nil?) rows)]}
  (let [vec-tab (mapv
                 (fn [{type :type cells :children}]
                   (if (= type :standard)
                     (mapv #(str " " (conv :table-row (:children %)) " ") cells)
                     []))
                 rows)
        cols-w (if-let [ne-vec-tab (seq (remove empty? vec-tab))]
                 (apply mapv
                        (fn [& cols]
                          (apply max (map viz-len cols)))
                        ne-vec-tab)
                 [])]
    (vec (concat [cols-w] vec-tab))))


(defn table-rule-str
  [cols-w]
  {:pre [(s/valid? (s/coll-of pos-int?) cols-w)]}
  (join "+" (map #(join (repeat % "-")) cols-w)))


(defn table-row-str
  [row cols-w]
  {:pre [(vector? row)
         (s/valid? (s/coll-of pos-int?) cols-w)]}
  (join "|"
        (map (fn [column-width cell-str]
               (str cell-str
                    (join (repeat (- column-width
                                     (viz-len cell-str))
                                  " "))))
             cols-w
             row)))


(defmethod sdn->org :table
  [table]
  (-> (let [[cols-w & vrep] (table->vec-rep table)]
        (->> (r/fold (r/monoid #(join "\n" [%1 %2]) str)
                     (r/map (comp (partial format "|%s|")
                                  #(cond (empty? cols-w) "" ;; <- no cols
                                         ;; Empty cols are rulers.
                                         (empty? %) (table-rule-str cols-w)
                                         :else (table-row-str % cols-w)))
                            vrep))
             (sdu/indent table-indentation)))
      (str/replace-first #"^\n" "")))


(defn fmt-cell-content
  "FIXME: Tables shouldn't have newlines or pipes
  but silently removing them is sub-optimal."
  [t-c-children-str]
  (str/replace t-c-children-str #"\n|\|" " "))


(defmethod sdn->org :table-cell
  [{:keys [tag children]}]
  (fmt-cell-content (conv tag children)))


(defmethod sdn->org :table-row
  [{:keys [tag type children]}]
  (if (= type :standard)
    (str "| " (join " |" (conv* tag children)) " |")
    ;; Ruler prefix.
    "|-"))


(defmethod sdn->org :link
  [{:keys [tag type path children]}]
  (format "[[%s]%s]"
          (sdu/fmt-link type path)
          (if (seq children)
            (format "[%s]" (str/trim (conv tag children)))
            "")))


(defmethod sdn->org :list-item
  [{b :bullet c :checkbox [{:keys [tag children]} item-tag] :children}]
  (let [itag (some->> item-tag
                      (:children)
                      (conv :item-tag))
        last-child-tag (->> children last :tag)
        last-child-kind (tag->kind last-child-tag)
        pref (str (str/trim b)
                  " "
                  (when itag (format "%s :: " itag) ""))]
    (apply str
           pref
           (->> children
                (conv tag)
                (sdu/indent (count pref))
                (str/triml)))))


(defmethod sdn->org :example
  [{value :value}]
  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n"
          (sdu/indent begin-end-indentation value)))


(defmethod sdn->org :src
  [{:keys [language value]}]
  (format "#+BEGIN_SRC %s\n%s#+END_SRC\n"
          language
          (sdu/indent begin-end-indentation value)))


(defmethod sdn->org :text
  [{value :value}]
  (sdu/fmt-text value))


(defmethod sdn->org :superscript
  [{:keys [tag children]}]
  (apply str "^" (conv* tag children)))


(defmethod sdn->org :subscript
  [{:keys [tag children]}]
  (apply str "_" (conv* tag children)))


(defmethod sdn->org :line-break
  [_]
  "\n")


(defmethod sdn->org :key-word
  [{:keys [key value]}]
  (format "#+%s: %s\n" key value))


(defmethod sdn->org :headline
  [{:keys [value children] :as hl}]
  (let [f-val (sdu/fmt-hl-val value)
        headline (-> hl
                     (update :path-id #(or % (sdu/hl-val->path-id-frag f-val)))
                     (assoc :value f-val)
                     (update :level #(or % 1)))
        tag (:tag headline)]
    (str (join (repeat (:level headline) "*"))
         " "
         (when (= tag :todo) "TODO ")
         f-val
         "\n"
         (conv tag
               (mapv #(if (n/headlines-tags (:tag %))
                        (sdu/assoc-level-and-path-id headline %)
                        %)
                     children)))))


(defmethod sdn->org :verbatim
  [{:keys [tag value]}]
  (let [token (emphasis-tokens tag)]
    (str token value token)))


(defmethod sdn->org :kbd
  [{:keys [tag value]}]
  (let [token (emphasis-tokens tag)]
    (str token (join " " value) token)))


(defmethod sdn->org :root
  [{:keys [tag] :as root}]
  (->> root
       (assoc-toc)
       (:children)
       (mapv #(if (n/headlines-tags (:tag %))
                (sdu/assoc-level-and-path-id %)
                  %))
       (conv tag)))


;;;; Pseudo-nodes

(defmethod sdn->org ::end
  [_]
  "")

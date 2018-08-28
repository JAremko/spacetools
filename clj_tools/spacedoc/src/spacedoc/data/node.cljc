(ns spacedoc.data.node
  (:require [spacedoc.util :as u]
            [clojure.set :refer [union map-invert]]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))


(def seps  #{\! \? \: \; \( \) \{ \} \, \. \- \\ \newline \space \tab})


(def link-type->prefix {:file "file:"
                        :http "http://"
                        :https "https://"
                        :custom-id "#"
                        :ftp "ftp://"})


(def max-headline-depth 5)


;;;; Helpers


(defn path->link-prefix
  [path]
  (->> (vals link-type->prefix)
       (filter (partial str/starts-with? path))
       (first)))


;; Headline stuff

(defn path-id?
  [val]
  (re-matches
   ;; forgive me Father for I have sinned
   #"^(?!.*[_/]{2}.*|^/.*|.*/$|.*[\p{Lu}].*)[\p{Nd}\p{L}\p{Pd}\p{Pc}/]+$"
   val))


(defn hl-val->gh-id-base
  [hl-value]
  (str "#"
       (-> hl-value
           (str/replace " " "-")
           (str/lower-case)
           (str/replace #"[^\p{Nd}\p{L}\p{Pd}\p{Pc}]" ""))))


(defn hl-val->path-id-frag
  [hl-value]
  (-> hl-value
      (str/lower-case)
      (str/replace #"[^\p{Nd}\p{L}\p{Pd}]" " ")
      (str/trim)
      (str/replace #"\s+" "_")))


(defn fill-hl
  "Give Headline placeholder a proper tag and fill all necessary key-vals."
  ([{tag :tag value :value :as headline}]
   (assoc headline
          :level 1
          :path-id (hl-val->path-id-frag value)))
  ([{p-level :level p-path-id :path-id :as parent-headline}
    {tag :tag value :value :as headline}]
   (let [hl-level (inc p-level)]
     (assoc headline
            :level hl-level
            :path-id (str p-path-id "/" (hl-val->path-id-frag value))))))


;; Defnode

(defn- gen-constructor-inner
  [tag doc alt]
  {:pre [(qualified-keyword? tag)]}
  (letfn [(sort-keys [ks] (sort (fn [a _] (if (= "children" (name a)) 1 -1)) ks))
          (keys->un-k->q-k [ks] (zipmap (map u/unqualify ks) ks))
          (sym-col [col] (mapv (comp symbol name) col))]
    (let [sorted-keys (sort-keys (u/map-spec->keys tag))
          un-k->q-k (keys->un-k->q-k sorted-keys)]
      (eval
       `(defn ~(symbol (str (name tag) (when alt "*")))
          ~doc
          ~(->> sorted-keys
                (sym-col)
                (remove #{'tag})
                (replace {'children ['& 'children]})
                (flatten)
                (vec))
          {:pre ~(mapv (fn [spec-key arg] `(s/valid? ~spec-key ~arg))
                       sorted-keys
                       (replace {'tag (u/unqualify tag)
                                 'children '(vec children)}
                                (sym-col sorted-keys)))
           :post [(s/valid? ~tag ~'%)]}
          ~(zipmap (keys un-k->q-k)
                   (replace {'tag (u/unqualify tag) 'children '(vec children)}
                            (sym-col (vals un-k->q-k)))))))))


(defn gen-constructor
  [qualified-node-tag doc]
  {:pre [(qualified-keyword? qualified-node-tag)]}
  (gen-constructor-inner qualified-node-tag doc false))


(defn gen-constructor*
  [qualified-node-tag doc]
  {:pre [(qualified-keyword? qualified-node-tag)]}
  (gen-constructor-inner qualified-node-tag doc true))


(defmulti node->spec-k :tag)


(defn all-tags
  []
  (set (remove #{:default} (keys (methods node->spec-k)))))


(defn- known-node?
  [tag]
  ((all-tags) tag))


(s/def ::known-node known-node?)
(defmethod node->spec-k :default [_] ::known-node)


(s/def ::node (s/multi-spec node->spec-k :tag))


(defn- alt-cons
  [tag]
  ({:link "`link`"
    :plain-list "`ordered-list` and `unordered-list`."
    :description "`description`"
    :headline "`headline`"
    :todo "`todo`"
    :table "`table`"}
   tag))


(defmacro defnode
  "Like `s/def` but also creates node constructor based on spec-form spec."
  [k spec-form]
  `(let [alt# ~(alt-cons (u/unqualify k))]
     (defmethod node->spec-k ~(u/unqualify k) [_#] ~k)
     (s/def ~k  ~spec-form)
     (gen-constructor-inner
      ~k
      (str (format "\"%s\" node constructor [auto-generated]." ~(name k))
           (some->> alt# (str "\nThe node has an alternative constructor: ")))
      alt#)))


;;;; Constructors


;; Document constructors

(defn conj-toc
  [body-node])


;; SDN node constructors

(defn unordered-list
  "Unordered \"plain-list\" node constructor."
  [items])


(defn ordered-list
  "ordered \"plain-list\" node constructor."
  [items])


(defn link
  "\"link\" node constructor."
  [path & children]
  {:pre  [(path->link-prefix path)]}
  (let [link-prefix (path->link-prefix path)
        link-type ((map-invert link-type->prefix) link-prefix)]
    {:tag :link
     :path (str/replace-first path link-prefix "")
     :type link-type
     :raw-link path
     :children (vec children)}))


;;; Nodes definitions

(load "node_spec")

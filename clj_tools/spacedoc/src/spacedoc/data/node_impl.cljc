(ns ^{:doc "Defnode implementation"}
    spacedoc.data.node-impl
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacedoc.data :as data]
            [spacedoc.util :as u]))


(defn- sdn-key-rank
  [sdn-key]
  {:pre [(keyword? sdn-key)(u/unqualified-ident? sdn-key)]
   :post [(integer? %)]}
  (or (sdn-key
       {:tag (Integer/MIN_VALUE)
        :key (inc Integer/MIN_VALUE)
        :value (+ 2 (Integer/MIN_VALUE))
        :type -2
        :path -1
        ;; Everything else here

        ;; Always last
        :children (Integer/MAX_VALUE)}
       0)))


;; (s/describe :spacedoc.data.node.list-item/children)

;; (s/describe :spacedoc.data.node.description/children)

;; (s/valid? (s/spec (s/cat :a int? :b int? :c (s/coll-of string?))) [1 2 "s" "s"])


;; (headline* "foo" 1 "foo" (section (paragraph (text "s"))))


(defn- map-spec->keys
  "Extremely naive way to scoop keywords from map-spec."
  [spec-form]
  (letfn [(sort-keys [ks] (sort-by (comp sdn-key-rank u/unqualify) ks))
          (map-spec->keys [sf] (some->> sf
                                        (tree-seq #(list? %) rest)
                                        (mapcat #(when (vector? %) %))
                                        (set)))]
    (-> spec-form map-spec->keys sort-keys)))


(defn- defnode-doc-string
  [k alt]
  (str (format "\"%s\" node constructor [auto-generated]."
               (name k))
       (some->> alt
                (str "\nThe node has an alternative "
                     "human friendly constructor: "))))


(defn- defonde-args
  [unq-keys->])


(defmacro defnode
  "Define node, its spec and constructor by SPEC-FORM. K is the spec key.
  ALT? is the name of alternative constructor.
  NOTE: This macro has limitations:
  - Currently it works only with `s/keys` and `s/merge` forms
  in the node spec without resolving sub-specs specified by keywords.
  See `map-spec->keys` implementation.
  - Children specs can only be `s/coll-of` or `s/cat` forms."
  ([k spec-form] (defnode &form &env k nil spec-form))
  ([k alt? spec-form]
   (let [tag (u/unqualify k)
         q-ks (map-spec->keys spec-form)
         arg-tmpl (mapv (comp symbol name) q-ks)
         ret-tmpl (replace {'children '(vec children)} arg-tmpl)
         f-name (symbol (str (name k) (when alt? "*")))
         tag-spec-k (keyword (str (namespace k) "." (name k)) "tag")]
     `(do
        ;; Register node
        (defmethod data/node->spec-k ~tag [_#] ~k)
        ;; Define tag spec
        (s/def ~tag-spec-k #{~tag})
        ;; Define node's spec
        (s/def ~k (s/merge (s/keys :req-un [~tag-spec-k]) ~spec-form))
        ;; Constructor function's spec
        (s/fdef ~f-name
          :args (s/cat ~@(interleave (mapv u/unqualify q-ks) q-ks))
          :ret ~k
          :fn (s/and
               #(= (-> % :args (conj :tag) count)
                   (-> % :ret keys count))
               #(= (-> % :args :children count)
                   (-> % :ret :children count))))
        ;; Constructor function's definition
        (defn ~f-name
          ;; Doc-string
          ~(defnode-doc-string k alt?)
          ;; Args
          ~(vec (flatten (replace {'children ['& 'children]}  arg-tmpl)))
          ;; pre/post conditions
          {:pre ~(mapv (fn [s-k arg] `(s/valid? ~s-k ~arg)) q-ks ret-tmpl)
           #_ :post #_ [(s/valid? ~k ~'%)]}
          ;; Returned value
          ~(merge {:tag tag} (zipmap (mapv u/unqualify q-ks) ret-tmpl)))))))

(ns spacetools.spacedoc.node-impl
  "Defnode implementation. Highly meh."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [spacetools.spacedoc.config :as conf]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.util :as sdu]))


(def unqualified-ident? (complement qualified-ident?))


(defn unqualify
  [symbol-or-keyword]
  {:pre [(ident? symbol-or-keyword)]}
  (if (unqualified-ident? symbol-or-keyword)
    symbol-or-keyword
    (when-let [name (name symbol-or-keyword)]
      (if (keyword? symbol-or-keyword)
        (keyword name)
        (symbol name)))))


(defn sdn-key-rank
  [sdn-key]
  {:pre [(keyword? sdn-key)(unqualified-ident? sdn-key)]
   :post [(integer? %)]}
  (sdn-key
   {:tag (Integer/MIN_VALUE)
    :key (inc Integer/MIN_VALUE)
    :value (+ 2 (Integer/MIN_VALUE))
    :type -2
    :path -1
    ;; Everything else here

    ;; Always last
    :children (Integer/MAX_VALUE)}
   0))


(defn map-spec->keys
  "Extremely naive way to scoop keywords from map-spec."
  [spec-form]
  (->> spec-form
       (tree-seq #(list? %) rest)
       (mapcat #(when (vector? %) %))
       (set)
       (sort-by (comp sdn-key-rank unqualify))))


(defn defnode-spec-args
  "If You read this - I'm sorry..."
  [q-keys]
  (let [key-map (zipmap (mapv unqualify q-keys) q-keys)
        q-keys-no-ch (remove #(#{(:children key-map)} %) q-keys)
        ch-s-f (some-> key-map :children s/get-spec s/form)]
    (concat (interleave (mapv unqualify q-keys-no-ch) q-keys-no-ch)
            (condp = (some-> ch-s-f first name)
              "coll-of" [:children `(s/+ ~(second ch-s-f))]
              "cat" (rest ch-s-f)
              nil []
              (throw (ex-info "Can't generate children args with:"
                              {:keys q-keys}))))))


(defn defnode-impl
  [k con? spec-form]
  (let [tag (unqualify k)
        q-ks (map-spec->keys spec-form)
        arg-tmpl (mapv (comp symbol name) q-ks)
        ret-tmpl (replace {'children '(vec children)} arg-tmpl)
        f-name (symbol (name k))
        tag-spec-k (keyword (str (namespace k) "." (name k)) "tag")
        arg-l (when con? (defnode-spec-args q-ks))]
    (concat
     `(do)
     `((defmethod sc/node->spec-k ~tag [_#] ~k)
       ;; Define tag spec
       (s/def ~tag-spec-k #{~tag})
       ;; Define node's spec
       (s/def ~k (s/merge (s/keys :req-un [~tag-spec-k]) ~spec-form)))
     (when con?
       `(;; Constructor function's spec
         (s/fdef ~f-name
           :args (s/cat ~@arg-l)
           :ret ~k)
         ;; Constructor function's definition
         (defn ^:autogenerated ~f-name
           ;; Doc-string
           ~(format "\"%s\" node constructor [AUTO-GENERATED]" k)
           ;; Args
           ~(vec (flatten (replace {'children ['& 'children]}  arg-tmpl)))
           ;; pre/post conditions
           {:pre ~(mapv (fn [s-k arg] `(s/valid? ~s-k ~arg)) q-ks ret-tmpl)
            :post [(s/valid? ~k ~'%)]}
           ;; Returned value
           ~(merge {:tag tag} (zipmap (mapv unqualify q-ks) ret-tmpl))))))))


(defmacro defnode
  "Define node, its spec and constructor by SPEC-FORM. K is the spec key.
  NOTE: This macro has limitations:
  - Currently it works only with `s/keys` and `s/merge` forms
    in the node spec and doesn't resolving sub-specs specified by keywords.
    See `map-spec->keys` implementation.
  - When generating constructors the children specs can only be
    `s/coll-of` or `s/cat` forms. To skip this step use `defnode*`."
  [k spec-form] (defnode-impl k true spec-form))


(defmacro defnode*
  "Same as `defnode` but doesn't create constructor."
  [k spec-form] (defnode-impl k nil spec-form))

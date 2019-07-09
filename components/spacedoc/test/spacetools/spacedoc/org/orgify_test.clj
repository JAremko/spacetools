(ns spacetools.spacedoc.org.orgify-test
  "Testing export to .org format."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.org.orgify :refer :all]
            [spacetools.spacedoc.util :as sdu]
            [spacetools.test-util.interface :as tu]))


(defmulti invariants
  "Given tested NODE and its ORG-STR(org string representation), returns true
  if the data stays consistent between formats."
  (fn [node org-str]
    (if-let [node-spec (s/get-spec (sc/node->spec-k node))]
      (cond
        ((complement s/valid?) node-spec node)
        (throw (ex-info "Invalid node" (s/explain-data node-spec node)))

        ((complement string?) org-str)
        (throw (IllegalArgumentException.
                (format "org-str: \"%s\" must be a String" org-str)))
        :else (:tag node))
      (throw (IllegalArgumentException.
              (format "node: \"%s\" must be a SDN node" node))))))


(defmethod invariants :text
  [{val :value} org-str]
  (str/includes? org-str (sdu/fmt-str val)))


(defmethod invariants :link
  [{:keys [type path]} org-str]
  (str/includes? org-str (sdu/fmt-link type path)))


(defmethod invariants :headline
  [{val :value} org-str]
  (str/includes? org-str (sdu/fmt-hl-val val)))


(defmethod invariants :default
  [node org-str]
  (and (map? node) (string? org-str)))


(st/instrument)


(doall
 (for [v (remove indirect-nodes (sc/all-tags))
       :let [node-name (name v)]]
   (eval
    `(binding [s/*recursion-limit* 2]
       (defspec ~(symbol (str node-name "-node->org-string"))
         ~(tu/samples 10)
         (testing (format (str "Any valid \"%s\" node can "
                               "be exported to the org format.")
                          ~node-name)
           (prop/for-all [node# (-> ~v
                                    (sc/tag->spec-k)
                                    (s/get-spec)
                                    (s/gen)
                                    (gen/no-shrink))]
                         (invariants node# (sdn->org node#)))))))))


(deftest tag->kind-fn
  (is (nil? (tag->kind :foo)))
  (is (nil? (tag->kind nil)))
  (is (= (tag->kind :paragraph) :block)))

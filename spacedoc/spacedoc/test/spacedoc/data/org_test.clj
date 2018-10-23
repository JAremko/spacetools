(ns spacedoc.data.org-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [spacedoc.data :as data]
            [spacedoc.data.org :refer :all]
            [spacedoc.shared :refer [samples]]
            [clojure.string :as str]))

(defmulti invariants
  (fn [node org-str]
    (if-let [node-spec (s/get-spec (data/node->spec-k node))]
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
  (str/includes? org-str (fmt-text val)))


(defmethod invariants :link
  [{:keys [raw-link type]} org-str]
  (str/includes? org-str (fmt-raw-link type raw-link)))


(defmethod invariants :headline
  [{val :value} org-str]
  (str/includes? org-str (fmt-hl-val val)))


(defmethod invariants :description
  [{val :value} org-str]
  (str/includes? org-str (fmt-hl-val val)))


(defmethod invariants :todo
  [{val :value} org-str]
  (str/includes? org-str (fmt-hl-val val)))


(defmethod invariants :default
  [node org-str]
  (and (map? node) (string? org-str)))


(doall
 (for [v (filter (complement indirect-nodes) (data/all-tags))
       :let [node-name (name v)]]
   (eval
    `(binding [s/*recursion-limit* 2]
       (defspec ~(symbol (str node-name "-node->org-string"))
         ~(samples 10)
         (testing (format (str "Any valid \"%s\" node can "
                               "be exported to the org format.")
                          ~node-name)
           (prop/for-all [node# (-> ~v
                                    (data/tag->spec-k)
                                    (s/get-spec)
                                    (s/gen)
                                    (gen/no-shrink))]
                         (invariants node# (sdn->org node#)))))))))

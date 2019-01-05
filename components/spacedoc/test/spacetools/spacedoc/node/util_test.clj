(ns spacetools.spacedoc.node.util-test
  "All public function in `spacetools.spacedoc.node` ns are node constructors.
  So we simply select them and generate tests based on node specs."
  (:require [clojure.core.reducers :as r]
            [clojure.data :refer [diff]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node] ;; Ensure node spec generation.
            [spacetools.spacedoc.node.util :refer :all]
            [spacetools.test-util.interface :as tu]))


(st/instrument)


(doall
 (for [v (vals (ns-publics 'spacetools.spacedoc.node.util))
       :let [f-name (str (:name (meta v)))]
       :when (function? (deref v))]
   (eval
    `(let [f-spec# (s/get-spec ~v)
           f-spec-args# (:args f-spec#)
           f-spec-ret# (:ret f-spec#)
           f-spec-desc# (some-> f-spec# s/form)]

       ;; All public functions properly speced?
       (deftest ~(symbol (str f-name "-has-spec"))
         (testing (str "Public function \"" ~v "\" properly speced.")
           (is (s/spec? f-spec#)
               (format (str "Public function \"%s`\" doesn't have spec.\n") ~v))
           (is (s/spec? f-spec-args#)
               (format "Function `%s` doesn't have :args spec.\n spec: \"%s\"\n"
                       ~v f-spec-desc#))
           (is (and (s/spec? f-spec-ret#)
                    (not= 'clojure.core/any? (s/form f-spec-ret#)))
               (format (str "Function `%s` doesn't have :ret spec or"
                            " its spec is `any?`.\n spec: \"%s\"\n")
                       ~v f-spec-desc#))))))))


(defn-spec depth->headline :spacetools.spacedoc.node/headline
  "Make headline dummy of given DEPTH and WIDTH."
  [depth pos-int? width pos-int?]
  ((fn rec [cur-depth cur-node]
     (assoc cur-node
            :children
            (if (< cur-depth depth)
              (r/reduce (r/monoid conj vector)
                        (->> {:tag :headline
                              :value (format "headline of depth: [%s]" cur-depth)
                              :children []}
                             (repeat)
                             (take width)
                             (r/map (partial rec (inc cur-depth)))))
              (->> {:tag :headline :value "bottom" :todo? true :children []}
                   (repeat)
                   (take width)
                   (vec)))))
   1 {:tag :headline :value "root"}))


(defn-spec hl-problems (s/nilable (s/coll-of map? :min-count 1))
  "Return true if X is a valid headline."
  [x any?]
  (:clojure.spec.alpha/problems
   (s/explain-data :spacetools.spacedoc.node/headline x)))


(deftest headline-utility-test
  (defspec valid-headline-gentest
    {:num-tests (tu/samples 10)}
    (prop/for-all
     [headline (gen/no-shrink (s/gen :spacetools.spacedoc.node/headline))]
     (testing "Generate valid headline"
       (is (empty? (hl-problems headline))
           "We generated valid headline."))
     (testing "headline? test"
       (is (headline? headline)
           "Valid headline is headline."))
     (testing "headline->depth test"
       (is (>= (cfg/max-headline-depth) (headline->depth headline) 1)
           "Valid headline depth should be in acceptable range."))
     (testing "clamp-headline-children test"
       (let [[removed added same] (diff headline (clamp-headline-children
                                                  (inc (cfg/max-headline-depth))
                                                  headline))]
         (is (every? nil? [removed added])
             "Clamping children to level over upper bound should do nothing."))
       (is (nil? (hl-problems (clamp-headline-children
                               (cfg/max-headline-depth)
                               headline)))
           "headline clamped to max valid level should also be valid.")
       (is (->> headline (clamp-headline-children 1)
                (:children)
                (filter headline?)
                (empty?))
           "headline clamped to 1 level shouldn't have headline children.")
       (let [[removed added same] (diff headline (clamp-headline-children
                                                  (headline->depth headline)
                                                  headline))]
         (is (every? nil? [removed added])
             "headline clamped to its original level should be the same."))))))

;; (when (and f-spec-args# f-spec-ret#)
;;   (binding [s/*recursion-limit* 2]
;;     (defspec ~(symbol (str f-name "-gentest"))
;;       {:num-tests ~(tu/samples 10)
;;        :reporter-fn (tu/make-f-spec-reper f-spec-ret# ~v ~f-name)}
;;       (testing "The function always returns valid result"
;;         (prop/for-all
;;          [args# (-> f-spec-args# (s/gen) (gen/no-shrink))]
;;          (s/valid? f-spec-ret# (apply ~v args#)))))))



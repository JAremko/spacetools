(ns spacedoc.data.node-test
  (:require #_ [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [spacedoc.data.node :refer :all]
            [spacedoc.shared :refer [samples make-f-spec-reper]]))


(doall
 (for [v (vals (ns-publics 'spacedoc.data.node))
       :let [f-name (str (:name (meta v)))]
       :when (function? (deref v))]
   (eval
    `(let [f-spec# (s/get-spec ~v)
           f-spec-args# (:args f-spec#)
           f-spec-ret# (:ret f-spec#)
           f-spec-desc# (some-> f-spec# s/form)]

       ;; All node constructors have specs?
       (deftest ~(symbol (str f-name "-has-spec"))
         (testing (str "Node constructor function \"" ~v "\" speced.")
           (is (s/spec? f-spec#)
               (format (str "Public function `%s` doesn't have spec\n"
                            "All public functions in the `spacedoc.data.node`"
                            " ns considered node constructors "
                            "and must be speced.\n")
                       ~v))
           (is (s/spec? f-spec-args#)
               (format "Function `%s` doesn't have :args spec.\n spec: \"%s\"\n"
                       ~v
                       f-spec-desc#))
           (is (and (s/spec? f-spec-ret#)
                    (not= 'clojure.core/any? (s/form f-spec-ret#)))
               (format (str "Function `%s` doesn't have :ret spec or "
                            "its spec is `any?`.\n spec: \"%s\"\n")
                       ~v
                       f-spec-desc#))))

       ;; [gentest] All node constructors produce valid values?
       (when (and f-spec-args# f-spec-ret#)
         (binding [s/*recursion-limit* 2]
           (defspec ~(symbol (str f-name "-gentest"))
             {:num-tests ~(samples 10)
              :reporter-fn (make-f-spec-reper f-spec-ret# ~v ~f-name)}
             (testing "The function always returns valid result"
               (prop/for-all
                [args# (-> f-spec-args#
                           (s/gen)
                           (gen/no-shrink))]
                (s/valid? f-spec-ret# (apply ~v args#)))))))))))

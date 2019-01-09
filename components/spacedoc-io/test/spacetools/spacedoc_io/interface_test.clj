(ns spacetools.spacedoc-io.interface-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc-io.core :refer [filesystem]]
            [spacetools.spacedoc-io.interface :as io]
            [spacetools.test-util.interface :as tu]))


(defmacro testing-io
  "Testing IO functions with fresh \"in-memory\" file-system.
  NAME is the name of the test (goes to `testing`).
  STRUCT is the file system initial structure - see `tu/create-fs`.
  Each element of TEST-FORMS has the shape: [OS BODY] where:
  OS is the operation system used in the test and mast be one of the
  keywords: `:unix` `:osx` `:windows`. BODY is the `testing` macro body."
  [name struct & test-forms]
  (conj (for [test-form test-forms
              :let [[os & body] test-form]]
          `(testing (format "Testing \"%s\" I/O function on \"%s\" OS"
                            ~name
                            (str/capitalize (name ~os)))
             (with-redefs [filesystem (tu/create-fs ~struct  ~os)]
               ~@body)))
        'do))


(st/instrument)


;; We're testing only interface of the component.
(deftest spacedoc-io
  (testing-io "absolute" []
              [:unix
               (is (= "/work/bar" (str (io/absolute "bar"))))]
              [:osx
               (is (= "/work/bar" (str (io/absolute "bar"))))]
              [:windows
               (is (= "C:\\work\\bar" (str (io/absolute "bar"))))])

  ;; (testing-io "rebase-path" [] :unix
  ;;             )
  ;; (testing-io "rebase-path" [] :unix
  ;;             )
  ;; (testing-io "rebase-path" [] :unix
  ;;             )
  ;; (testing-io "*spit" [] :unix
  ;;             )
  ;; (testing-io "sdn-file?" [] :unix
  ;;             )
  ;; (testing-io "directory?" [] :unix
  ;;             )
  ;; (testing-io "*sdn-fps-in-dir" [] :unix
  ;;             )
  ;; (testing-io "*fp->sdn" [] :unix
  ;;             )
  ;; (testing-io "try-m->output" [] :unix
  ;;             )
  ;; (testing-io "*read-cfg-overrides" [] :unix
  ;;             )
  )
;; => #'spacetools.spacedoc-io.interface-test/spacedoc-io


;; (doall
;;  (for [v (vals (ns-publics 'spacetools.spacedoc.node))
;;        :let [f-name (str (:name (meta v)))]
;;        :when (function? (deref v))]
;;    (eval
;;     `(let [f-spec# (s/get-spec ~v)
;;            f-spec-args# (:args f-spec#)
;;            f-spec-ret# (:ret f-spec#)
;;            f-spec-desc# (some-> f-spec# s/form)]

;;        ;; All node constructors have specs?
;;        (deftest ~(symbol (str f-name "-has-spec"))
;;          (testing (str "Node constructor function \"" ~v "\" speced.")
;;            (is (s/spec? f-spec#)
;;                (format (str "Public function `%s` doesn't have spec\n"
;;                             "All public functions in the `spacetools.spacedoc.node`"
;;                             " ns considered node constructors "
;;                             "and must be speced.\n")
;;                        ~v))
;;            (is (s/spec? f-spec-args#)
;;                (format "Function `%s` doesn't have :args spec.\n spec: \"%s\"\n"
;;                        ~v
;;                        f-spec-desc#))
;;            (is (and (s/spec? f-spec-ret#)
;;                     (not= 'clojure.core/any? (s/form f-spec-ret#)))
;;                (format (str "Function `%s` doesn't have :ret spec or "
;;                             "its spec is `any?`.\n spec: \"%s\"\n")
;;                        ~v
;;                        f-spec-desc#))))

;;        ;; [gentest] All node constructors produce valid values?
;;        (when (and f-spec-args# f-spec-ret#)
;;          (binding [s/*recursion-limit* 2]
;;            (defspec ~(symbol (str f-name "-gentest"))
;;              {:num-tests ~(tu/samples 10)
;;               :reporter-fn (tu/make-f-spec-reper f-spec-ret# ~v ~f-name)}
;;              (testing "The function always returns valid result"
;;                (prop/for-all
;;                 [args# (-> f-spec-args#
;;                            (s/gen)
;;                            (gen/no-shrink))]
;;                 (s/valid? f-spec-ret# (apply ~v args#)))))))))))

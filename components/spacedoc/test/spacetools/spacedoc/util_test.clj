(ns spacetools.spacedoc.util-test
  "Testing general purpose helpers."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.rpl.specter :refer :all]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.core :as sc]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :refer :all]
            [spacetools.test-util.interface :as tu]))


(st/instrument)


(defspec ^:slow indent-gen
  {:num-tests (tu/samples 10)}
  (prop/for-all
   [indentation gen/nat
    s-sample (gen/fmap str/join
                       (gen/tuple gen/string-alphanumeric
                                  (gen/elements ["\n" ""])
                                  gen/string-alphanumeric
                                  (gen/elements ["\n" ""])
                                  gen/string-alphanumeric
                                  (gen/elements ["\n" ""])
                                  gen/string-alphanumeric
                                  (gen/elements ["\n" ""])))]
   (is (->> s-sample
            (indent indentation)
            (str/split-lines)
            ;; Empty lines aren't indented.
            (remove empty?)
            (every? #(= (- (count %)
                           (count (str/triml %)))
                        indentation))))))


(deftest node->children-tag-s-fn
  (let [text (n/text "foo")
        section (-> text n/paragraph n/section)
        headline (n/todo "bar")
        root (n/root "foo" #{} section headline)]
    (is (= (node->children-tag-s text) #{}))
    (is (= (node->children-tag-s section) #{:paragraph}))
    (is (= (node->children-tag-s root) #{:section :headline}))))


(deftest valid-node?-fn
  (are [pred x] (pred (valid-node? x))
    true? (n/text "foo")
    true? (n/root "foo" #{} (n/todo "bar"))
    false? "baz"
    false? {:tag :qux})
  (let [set-roots-first-hl-val
        (fn [new-val r-node] (setval [:children FIRST :value] new-val r-node))
        test-node (n/root "foo" #{} (n/todo "quux"))]
    (is (valid-node? (set-roots-first-hl-val
                      "quuz"
                      test-node)))
    (is (not (valid-node? (set-roots-first-hl-val
                           :not-a-string
                           test-node))))))


(deftest fmt-problem-fn
  (let [text (n/text "foo")
        text-spec-form (s/form ::n/text)
        problem {:foo :bar}]
    (are [pred x] (pred (str/includes? (fmt-problem text problem) x))
      true? ":foo"
      true? ":bar"
      true? ":text"
      true? (str text-spec-form))))


(deftest explain-deepest-fn
  (let [good-node (->> "baz"
                       n/text
                       (n/paragraph (n/text "bar"))
                       n/section
                       (n/headline "foo"))
        bad-node (setval [:value] :bad-hl-val good-node)
        double-bad-node (setval [:children FIRST
                                 :children FIRST
                                 :children LAST
                                 :value]
                                :bad-last-text-val
                                bad-node)
        triple-bad-node (setval [:children FIRST
                                 :children FIRST
                                 :children FIRST
                                 :value]
                                :bad-first-text-val
                                double-bad-node)]
    (is (nil? (explain-deepest good-node)))
    (is (some? (explain-deepest {:tag :unknown-node})))
    (is (= (-> "text"
               (n/text)
               (assoc :value :bad-text-val)
               explain-deepest
               :clojure.spec.alpha/value
               :value)
           :bad-text-val))
    (are [b-node b-val]
        (= (-> b-node explain-deepest :clojure.spec.alpha/value :value)
           b-val)
      bad-node :bad-hl-val
      double-bad-node :bad-last-text-val
      triple-bad-node :bad-first-text-val)))


(deftest relation-fn
  (is (= (relation (n/text "foo")) {:text #{}}))
  (is (thrown? Exception (relation {:not-a-tag :text})))
  (is (= (relation (n/section (n/paragraph (n/text "foo")
                                           (n/text "bar")
                                           (n/verbatim "baz"))))
         {:section #{:paragraph}
          :paragraph #{:verbatim :text}
          :text #{}
          :verbatim #{}})))


(deftest relations-fn
  (is (thrown? Exception (relations (n/text "foo"))))
  (is (= (relations [(n/text "foo")]) {:text #{}}))
  (is (empty? (relations [])))
  (is (thrown? Exception (relations [{:not-a-tag :text}])))
  (let [node-a (n/section (n/paragraph (n/text "foo")
                                       (n/text "bar")
                                       (n/verbatim "baz")))
        node-b (n/paragraph (n/line-break))]
    (is (= (relations [node-a]) {:section #{:paragraph}
                                 :paragraph #{:verbatim :text}
                                 :text #{}
                                 :verbatim #{}}))
    (is (= (relations [node-b]) {:paragraph #{:line-break}
                                 :line-break #{}}))
    (is (= (relations [node-a node-b])
           {:section #{:paragraph}
            :paragraph #{:verbatim :text :line-break}
            :text #{}
            :verbatim #{}
            :line-break #{}}))))


(deftest regex-pat?-fn
  (is (regex-pat? #"foo"))
  (is (not (regex-pat? "bar")))
  (is (regex-pat? (re-pattern "foo"))))


(deftest fmt-str-fn
  (is (= (fmt-str "foo") "foo"))
  (is (= (fmt-str {#"foo" "bar"} "foo") "bar"))
  (is (= (fmt-str "key-bindings") "key bindings"))
  (is (= (fmt-str (cfg/text-rep-map) "key-bindings") "key bindings"))
  (is (= (fmt-str {} "key-bindings") "key-bindings")))


(deftest fmt-link-fn
  (testing "Formatting of custom-id links"
    (are [link-type pre pos] (= (fmt-link link-type pre) pos)
      :custom-id "foo" "foo"
      :custom-id "Foo" "foo"
      :custom-id "#key-bindings" "#key-bindings"
      :custom-id "keybindings" "key-bindings"
      :custom-id "#keybindings" "#key-bindings")
    (is (= (fmt-link {} :custom-id "keybindings") "keybindings"))
    (is (= (fmt-link {#"foo" "bar"} :custom-id "foo") "bar")))

  (testing "Formatting of non-custom-id links (currently noop)"
    (are [link-type pre pos] (= (fmt-link link-type pre) pos)
      :qux "foo" "foo"
      :qux "Foo" "Foo"
      :qux "#key-bindings" "#key-bindings"
      :qux "keybindings" "keybindings"
      :qux "#keybindings" "#keybindings")
    (is (= (fmt-link {} :custom-id "keybindings") "keybindings"))
    (is (= (fmt-link {#"foo" "bar"} :qux "foo") "foo"))))


(deftest fmt-hl-val-fn
  (is (= (fmt-hl-val "foo") "foo"))
  (is (= (fmt-hl-val "key-bindings") "key bindings"))
  (is (= (fmt-hl-val {#"foo" "bar"} "foo") "bar"))
  (is (= (fmt-hl-val {} "key-bindings") "key-bindings"))
  (testing "toc headline shouldn't be altered."
    (is (= (fmt-hl-val (cfg/toc-hl-val)) (cfg/toc-hl-val)))
    (is (= (fmt-hl-val {(-> (cfg/toc-hl-val)
                            (java.util.regex.Pattern/quote)
                            re-pattern)
                        ""}
                       (cfg/toc-hl-val))
           (cfg/toc-hl-val)))))


(deftest remove-invalid-fn
  (let [valid (n/root "foo" #{} (n/todo "bar"))
        invalid-node (dissoc (n/headline "foo" (n/todo "bar")) :children)
        invalid-simpl (assoc-in valid [:children 1] {:tag :text})
        valid-compl (n/root "foo" #{} (->> (n/todo "baz")
                                           (n/headline "bar")
                                           (n/headline "qux")))
        invalid-compl (assoc-in valid-compl [:children 1 :children 1] 42)]
    (is (nil? (remove-invalid invalid-node)))
    (is (tu/identity? remove-invalid valid))
    (is (= valid (remove-invalid invalid-simpl)))
    (is (= valid-compl (remove-invalid invalid-compl)))))

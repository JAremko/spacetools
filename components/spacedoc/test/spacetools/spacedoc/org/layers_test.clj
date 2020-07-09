(ns spacetools.spacedoc.org.layers-test
  "Testing generation of layers.org SDN structure."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [orchestra.spec.test :as st]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.org.layers :refer :all]
            [spacetools.test-util.interface :as tu]))

;; Test vals
(def test-text "test text from test-text var")
(def test-children (->> test-text n/text n/paragraph n/section vector))
(def test-description-text test-text)
(def test-description (apply n/description test-children))


;; Helpers
(defn contains-string?
  "Returns true if TREE EDN structure contains S string at any level."
  [s tree]
  (->> tree
       (tree-seq (some-fn map? vector?) #(or (:children %) %))
       vec
       (some #{s})
       some?))


(defn headline?
  "Returns true if X is a valid headline node."
  [x]
  (s/valid? :spacetools.spacedoc.node/headline x))


(defn make-root
  "Make simple root for testing."
  [& children]
  (apply n/root "Test root" #{} children))


(defn tags->tag->tag-descr
  "Create map of tags.
  See :valid-tags of `spacetools.spacedoc.config/default-config`"
  [tags]
  (->> tags
       (map (juxt identity (partial format "%s tag")))
       (into {})))


(defn has-n-children?
  "Returns true if sdn NODE has N number of children."
  [n node]
  (= n (count (:children node))))


(defn count-n?
  "Returns true if COLL collection's  `count` is N."
  [n coll]
  (= n (count coll)))


(def single?
  "Returns true if supplied collection has a singe element."
  (partial count-n? 1))


(st/instrument)


(deftest root->description-fn
  (is (nil? (root->description (n/root "foo" #{} (n/todo "bar"))))
      "The function should return nil if SDN doesn't have description.")
  (is (every? #(= (:children %) test-children)
              (map (comp root->description (partial apply make-root))
                   [[test-description]
                    [test-description
                     (->> "second desc"
                          n/text
                          n/paragraph
                          n/section
                          n/description)]
                    [(n/todo "bar") test-description]]))
      "Test if every extracted description has test children."))


(deftest capitalize-first-fn
  (are [s res] (= (capitalize-first s) res)
    " "   " "
    ""    ""
    "foo" "Foo"
    "FOO" "FOO"
    "Foo" "Foo"
    "fOO" "FOO"
    " foo" " foo"))


(defspec capitalize-first-gen
  {:num-tests (tu/samples 10)}
  (prop/for-all
   [s gen/string-alphanumeric]
   (is (= (first (capitalize-first s))
          (first (str/upper-case s))))
   (is (= (rest (capitalize-first s))
          (rest s)))))


(deftest describe-fn
  (let [source-link "source"
        asc-source #(assoc % :source source-link)
        no-link? (partial contains-string? missing-source-link-text)
        no-desc? (partial contains-string? missing-description-text)]
    ;; No link no description.
    (is ((every-pred no-link? no-desc?) (describe (make-root (n/todo "foo"))))
        (str "when supplied SDN doesn't have link and description, "
             "the function's output should include their placeholders."))
    ;; No source link but has description.
    (is (every? (every-pred no-link?
                            (complement no-desc?)
                            (partial contains-string? test-description-text))
                (map describe
                     [(make-root test-description)
                      (make-root (n/todo "foo") test-description)]))
        (str "When only source link is missing, the function's output "
             "should include source link placeholder "
             "but not description placeholder - "
             "description representation should be included instead."))
    ;; No description but has source link.
    (is ((every-pred no-desc?
                     (complement no-link?)
                     (partial contains-string? source-link))
         (describe (asc-source (make-root (n/todo "foo")))))
        (str "When only description is missing, the function's output "
             "should include description placeholder "
             "but not source link placeholder - "
             "source link should be included instead."))
    ;; Both has description and source link.
    (is ((every-pred (complement no-desc?)
                     (complement no-link?)
                     (partial contains-string? source-link)
                     (partial contains-string? test-description-text))
         (describe (asc-source (make-root test-description))))
        (str "When both source link and description are present in the input - "
             "no placeholders should be inserted into the function's output. "
             "Instead, source link and description representation "
             "should be included."))))


(deftest wrap-in-hl-fn
  (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                   (fn [] {"foo" "bar"})}
    #(do (is (= "bar" ((cfg/valid-tags) "foo"))
             "You should never ever see this message.")
         (is (= (wrap-in-hl "foo" test-children)
                (apply n/headline "bar" test-children)))
         (is (let [invalid-key "baz"]
               (= (:value (wrap-in-hl invalid-key test-children))
                  (fmt-invalid-tag invalid-key)))))))


(defspec ^:slow wrap-in-hl-gen
  {:num-tests (tu/samples 5)}
  (binding [s/*recursion-limit* 2]
    (prop/for-all
     [valid-tags (->> :spacetools.spacedoc.config/valid-tags
                      s/gen
                      (gen/such-that seq)
                      gen/no-shrink)
      hl-children (->> :spacetools.spacedoc.node.headline/children
                       s/gen
                       gen/no-shrink)]
     (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                      (fn [] valid-tags)}
       #(let [key (rand-nth (keys valid-tags))
              val (valid-tags key)]
          (is (= (wrap-in-hl key hl-children)
                 (apply n/headline val hl-children))))))))


(deftest merge-same-hls-fn
  (let [hl-foo-a (n/headline "foo" (n/todo "A"))
        hl-foo-b (n/headline "foo" (n/todo "B"))
        hl-foo-a+b (n/headline "foo"
                               (n/todo "A")
                               (n/todo "B"))
        hl-bar-a (n/headline "bar" (n/todo "A"))
        hl-bar-b (n/headline "bar" (n/todo "B"))
        hl-bar-c (n/headline "bar" (n/todo "C"))
        hl-bar-a+b (n/headline "bar"
                               (n/todo "A")
                               (n/todo "B"))
        hl-bar-a+b+c (n/headline "bar"
                                 (n/todo "A")
                                 (n/todo "B")
                                 (n/todo "C"))
        hl-baz-a (n/headline "baz" (n/todo "A"))]
    (testing "Empty collection is a valid argument."
      (merge-same-hls []))
    (testing "Singe headline isn't modified."
      (is (= (merge-same-hls [hl-foo-a])
             [hl-foo-a])))
    (testing "Different headlines aren't modified."
      (is (= (merge-same-hls [hl-foo-a hl-bar-a])
             [hl-foo-a hl-bar-a])))
    (testing "Two headlines with a same hl text are merged."
      (is (single? (merge-same-hls [hl-foo-a hl-foo-b]))
          "Returned collection have a single element.")
      (is (= (merge-same-hls [hl-foo-a hl-foo-b]) [hl-foo-a+b])
          "Children of the input sequences concatenated.")
      (is (= (merge-same-hls [hl-foo-a hl-bar-a hl-foo-b])
             (merge-same-hls [hl-foo-a hl-foo-b hl-bar-a])
             [hl-foo-a+b hl-bar-a])
          "An intermediate different headline moved to the right."))
    (testing "Three headlines with a same hl text are merged."
      (is (single? (merge-same-hls [hl-bar-a hl-bar-b hl-bar-c]))
          "Returned collection have a single element.")
      (is (= (merge-same-hls [hl-bar-a hl-bar-b hl-bar-c])
             [hl-bar-a+b+c])
          "Children of the input sequences are be concatenated.")
      (is (= (merge-same-hls [hl-foo-a hl-bar-a hl-bar-b hl-bar-c hl-baz-a])
             [hl-foo-a hl-bar-a+b+c hl-baz-a])
          "Wrapping the sequence in other headlines doesn't affect merging."))
    (testing "Two pair of the same headlines are merged."
      (is (count-n? 2 (merge-same-hls [hl-foo-a hl-foo-b hl-bar-a hl-bar-b]))
          "Returned collection should have two elements.")
      (is (= (merge-same-hls [hl-foo-a hl-foo-b hl-bar-a hl-bar-b])
             (merge-same-hls [hl-foo-a hl-bar-a hl-foo-b hl-bar-b])
             [hl-foo-a+b hl-bar-a+b])
          "Children of the input sequences concatenated."))))


(deftest query-node?-fn
  (is (query-node? {"foo" []}))
  (is (query-node? {"foo" ["bar"]}))
  (is (query-node? {"foo" ["bar" "baz"]}))
  (is (not (query-node? {"foo" ["bar"] "baz" ["qux"]})))
  (is (not (query-node? ["bar"]))))


(deftest query-node-parent?-fn
  (is (query-node-parent? {"foo" ["bar"]}))
  (is (query-node-parent? {"foo" ["bar" "baz"]}))
  (is (not (query-node-parent? {"foo" []})))
  (is (not (query-node-parent? "foo"))))


(deftest query-fragment->tag-fn
  (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                   (fn [] {"foo" "bar"})}
    #(do (is (= "foo"
                (query-fragment->tag "foo")
                (query-fragment->tag {"foo" []})
                (query-fragment->tag {"foo" ["qux"]})))
         (is (thrown? Exception (query-fragment->tag "qux"))))))


(deftest layers-query-shaper-fn
  (testing "Minimal query with simple docs"
    (let [tags (tags->tag->tag-descr ["foo" "bar"])]
      (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                       (constantly tags)}
        #(let [doc (n/root "title" #{"foo"} (n/todo "bar"))]
           (is (thrown? Exception (layers-query-shaper [doc doc] {"foo" []}))
               "All docs must be unique.")
           (are [docs query shaped-pred leftover-pred]
               (let [{:keys [shaped leftover]}
                     (layers-query-shaper docs query)]
                 (and (shaped-pred shaped) (leftover-pred leftover)))
             ;; docs query shaped leftover
             [] {"foo" []} nil? empty?
             [doc] {"foo" []} some? empty?
             [doc] {"foo" []} headline? empty?
             [doc] {"foo" []} (fn [hl] (= (tags "foo") (:value hl))) empty?
             [doc] {"foo" []} (partial has-n-children? 1) empty?
             [doc] {"bar" []} nil? seq
             [doc] {"bar" []} nil? single?
             [doc] {"bar" []} nil? (fn [lft]
                                     (-> lft first :tags (= #{"foo"}))))))))
  (testing "Complex query with simple docs"
    (let [tags (tags->tag->tag-descr ["foo" "bar" "baz" "qux"])]
      (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                       (constantly tags)}
        #(let [doc-foo (n/root "title foo" #{"foo"} (n/todo "child foo"))
               doc-bar (n/root "title bar" #{"bar"} (n/todo "child bar"))
               doc-foo+bar (n/root "title foo bar"
                                   #{"foo" "bar"}
                                   (n/todo "child foo bar"))
               doc-foo+bar+baz (n/root "title foo bar baz"
                                       #{"foo" "bar" "baz"}
                                       (n/todo "child foo bar baz"))]
           (are [docs query shaped-pred leftover-pred]
               (let [{:keys [shaped leftover]}
                     (layers-query-shaper docs query)]
                 (and (shaped-pred shaped) (leftover-pred leftover)))
             ;; 2 docs have both foo and bar tags.
             [doc-foo doc-bar doc-foo+bar] {"foo" ["bar"]}
             headline? (partial count-n? 2)

             ;; 2 docs have both bar and foo tags.
             [doc-foo doc-bar doc-foo+bar] {"bar" ["foo"]}
             headline? (partial count-n? 2)

             ;; 2 docs with foo and bar tags + one doc that has only foo tag.
             [doc-foo doc-bar doc-foo+bar] {"foo" ["bar" "foo"]}
             headline? single?

             ;; Only one doc with baz tag
             [doc-foo doc-foo+bar doc-foo+bar+baz] {"baz" []}
             (partial has-n-children? 1) (partial count-n? 2)

             ;; All docs pulled into the top headline
             [doc-foo doc-foo+bar doc-foo+bar+baz] {"foo" []}
             (partial has-n-children? 3) empty?

             ;; All docs pulled into a singe sub headline.
             [doc-foo doc-foo+bar doc-foo+bar+baz] {"foo" ["foo"]}
             (partial has-n-children? 1) empty?

             [doc-foo doc-foo+bar doc-foo+bar+baz] {"foo" ["foo"]}
             (fn [hl] (->> hl :children first (has-n-children? 3))) empty?

             ;; Docs with tag foo and bar pulled into the first
             ;; sub headline and the rest of docs with foo tag
             ;; into the second.
             [doc-foo doc-foo+bar doc-foo+bar+baz] {"foo" ["bar" "foo"]}
             (fn [hl] (->> hl :children first (has-n-children? 2))) empty?

             [doc-foo doc-foo+bar doc-foo+bar+baz] {"foo" ["bar" "foo"]}
             (fn [hl] (->> hl :children second (has-n-children? 1))) empty?

             ;; Nested query selects singe document
             [doc-foo doc-foo+bar doc-foo+bar+baz] {"foo" [{"bar" ["baz"]}]}
             (partial has-n-children? 1) (partial count-n? 2)

             ;; Query selects singe document by nested part and then
             ;; the rest docs with "foo" tag.
             [doc-foo doc-foo+bar doc-foo+bar+baz]
             {"foo" [{"bar" ["baz"]} "foo"]}
             (partial has-n-children? 2) empty?

             [doc-foo doc-foo+bar doc-foo+bar+baz]
             {"foo" [{"bar" ["baz"]} "foo"]}
             (fn [hl] (->> hl :children first (has-n-children? 1)))
             empty?)))))

  (testing "Minimal query with complex docs"
    (let [tags (tags->tag->tag-descr ["foo"])]
      (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                       (constantly tags)}
        #(let [src "/foo/bar"
               d-simple (n/root "title" #{"foo"} (n/todo "bar"))
               d-with-src (assoc d-simple :source src)
               d-with-desc (update-in d-simple [:children]
                                      (partial into [test-description]))
               d-full (merge d-with-src d-with-desc)]
           (is (contains-string? missing-description-text
                                 (layers-query-shaper [d-simple] {"foo" []}))
               "With no description its placeholder must be present.")
           (is ((complement (partial contains-string? test-text))
                (layers-query-shaper [d-simple] {"foo" []}))
               "Without description there shouldn't be description text.")
           (is (every? (partial contains-string? test-text)
                       [(layers-query-shaper [d-with-desc] {"foo" []})
                        (layers-query-shaper [d-full] {"foo" []})])
               "Description text should be present when it is present.")
           (is ((complement (partial contains-string? src))
                (layers-query-shaper [d-simple] {"foo" []}))
               "Without source there shouldn't be source path.")
           (is ((complement (partial contains-string? src))
                (layers-query-shaper [d-simple] {"foo" []}))
               "Without source there shouldn't be source path.")
           (is (every? (partial contains-string? src)
                       [(layers-query-shaper [d-with-src] {"foo" []})
                        (layers-query-shaper [d-full] {"foo" []})])
               "With source it should be present in the out put."))))))


(deftest layers-sdn-fn
  (with-redefs-fn {#'spacetools.spacedoc.config/valid-tags
                   (constantly (tags->tag->tag-descr ["foo"]))
                   #'spacetools.spacedoc.config/layers-org-query
                   (constantly {"foo" []})}
    #(let [src "/foo/bar"
           d-simple (n/root "title" #{"foo"} (n/todo "bar"))
           d-with-src (assoc d-simple :source src)
           d-with-desc (update-in d-simple [:children]
                                  (partial into [test-description]))
           d-full (merge d-with-src d-with-desc)]
       (is (every? (partial s/valid? :spacetools.spacedoc.node/root)
                   (map layers-sdn
                        [[]
                         [d-simple]
                         [d-with-src]
                         [d-with-desc]
                         [d-full]
                         [d-full (n/root "title 2" #{"foo"} (n/todo "baz"))]]))
           "The function should always return valid root.")
       (testing "layers-query-shaper integration."
         (is (contains-string? missing-description-text
                               (layers-sdn [d-simple]))
             "With no description its placeholder must be present.")
         (is ((complement (partial contains-string? test-text))
              (layers-sdn [d-simple]))
             "Without description there shouldn't be description text.")
         (is (every? (partial contains-string? test-text)
                     [(layers-sdn [d-with-desc])
                      (layers-sdn [d-full])])
             "Description text should be present when it is present.")
         (is ((complement (partial contains-string? src))
              (layers-sdn [d-simple]))
             "Without source there shouldn't be source path.")
         (is ((complement (partial contains-string? src))
              (layers-sdn [d-simple]))
             "Without source there shouldn't be source path.")
         (is (every? (partial contains-string? src)
                     [(layers-sdn [d-with-src])
                      (layers-sdn [d-full])])
             "With source it should be present in the out put.")))))

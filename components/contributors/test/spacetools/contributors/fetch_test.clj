(ns spacetools.contributors.fetch-test
  (:use org.httpkit.fake)
  (:require [cats.monad.exception :as exc :refer [success? failure?]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [orchestra.spec.test :as st]
            [org.httpkit.client :as http]
            [spacetools.contributors.fetch :as f]))


(st/instrument)


(deftest *first->l-p-url-fn
  (let [b-url "https://api.github.com/"
        link-h-f "Link: <%sfoo?page=42>; rel=\"last\""
        f-p-url (str b-url "foo?page=1")
        l-p-url (str b-url "foo?page=42")]
    (testing "Testing last page URL retrieval with proper URL."
      (with-fake-http [{:url f-p-url} {:status 200
                                       :headers {:link (format link-h-f b-url)}
                                       :body "baz"}]
        (is (success? (f/*first->last-page-url f-p-url)))
        (is (= l-p-url @(f/*first->last-page-url f-p-url)))))

    (testing "Testing last page URL retrieval with 404 response."
      (with-fake-http [{:url f-p-url} {:status 404
                                       :headers {:link (format link-h-f b-url)}
                                       :body "baz"}]
        (is (failure? (f/*first->last-page-url f-p-url)))))

    (testing "Testing last page URL retrieval with broken link headder reps"
      (with-fake-http
        [{:url f-p-url} {:status 200 :headers {:link "foo"} :body "baz"}]
        (is (failure? (f/*first->last-page-url f-p-url)))))
    (testing "Testing last page URL retrieval without link headder resp"
      (with-fake-http
        [{:url f-p-url} {:status 200 :body "baz"}]
        (is (failure? (f/*first->last-page-url f-p-url)))))))

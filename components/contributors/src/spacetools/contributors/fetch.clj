(ns spacetools.contributors.fetch
  "Fetch contributors of a repository from GitHub."
  (:require [cats.core :as m]
            [cats.monad.exception :as exc]
            [cheshire.core :as json]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [org.httpkit.client :as client]
            [spacetools.fs-io.interface :refer [exception-of?]]))


(s/def ::github-api-url
  (s/and string?
         #(re-matches #"https://api.github.com/.*" %)))

(s/def ::github-api-paged-url
  (s/and string?
         #(re-matches #"https://api.github.com/.*(:?&|!)page=\d+.*" %)))

(s/def ::github-api-first-paged-url
  (s/and string?
         #(re-matches #"https://api.github.com/.*(:?&|!)page=1.*" %)))

(s/def ::github-api-contributors-url
  (s/and string?
         #(re-matches #"https://api.github.com/repositories/.*/contributors"
                      %)))


(defn-spec *first->last-page-url (exception-of? ::github-api-paged-url)
  [url ::github-api-first-paged-url]
  (exc/try-on
   (let [{{links :link} :headers status :status} @(client/head url)]
     (if-let [err (cond (not= status 200)
                        (ex-info "API response status code isn't 200"
                                 {:url url :status status})
                        (empty? links)
                        (ex-info "Can't find link header in the response"
                                 {:url url}))]
       (throw err)
       (or (last (re-find #"<([^>]+)>; rel=\"last\"" links))
           (throw (ex-info "Can't find last page for URL"
                           {:url url :link-header links})))))))


(defn-spec url->page-number (s/nilable pos-int?)
  [url ::github-api-paged-url]
  (last (re-find #"(:?&|!)page=(\d+)" url)))


(defn-spec *url->body (exception-of? vector?)
  [url ::github-api-url]
  (exc/try-on
   (let [{:keys [body status]} @(client/get url)]
     (if-let [err (cond (not= status 200)
                        (ex-info "API response status code isn't 200"
                                 {:url url :status status})
                        (empty? body)
                        (ex-info "response body is empty"
                                 {:url url}))]
       (throw err)
       body))))


(alias 'ctrb-u (create-ns 'spacetools.contributors.contributor.user))

(s/def ::ctrb-u/login (s/and string? (complement str/blank?)))
(s/def ::ctrb-u/contributions pos-int?)
(s/def ::ctrb-u/avatar_url (s/and string? #(re-matches #"https://.*" %)))
(s/def ::ctrb-u/type #{"User"})


(alias 'ctrb-a (create-ns 'spacetools.contributors.contributor.anon))

(s/def ::ctrb-a/name (s/and string? (complement str/blank?)))
(s/def ::ctrb-a/contributions pos-int?)
(s/def ::ctrb-a/type #{"Anonymous"})

(s/def ::contributor
  (s/or :github-user (s/keys :req-un [::ctrb-u/login
                                      ::ctrb-u/contributions
                                      ::ctrb-u/avatar_url
                                      ::ctrb-u/type])
        :anonymous (s/keys :req-un [::ctrb-a/name
                                    ::ctrb-a/contributions
                                    ::ctrb-a/type])))


(defn-spec *fetch-contributors (exception-of? (s/coll-of ::contributor))
  [url ::github-api-contributors-url]
  (let [url-infix "?anon=1&per_page=100&page="]
    (exc/try-on
     (m/mlet [lp-url (*first->last-page-url (str url url-infix 1))
              p-bodies (m/sequence
                        (map *url->body
                             (->> lp-url
                                  url->page-number
                                  Integer.
                                  (range 1)
                                  (map #(str url url-infix %)))))]
             (mapcat #(json/decode-strict % (fn [k] (keyword (.toLowerCase k))))
                     p-bodies)))))

;; (def test-data (vec @(*fetch-contributors "https://api.github.com/repositories/7212645/contributors")))

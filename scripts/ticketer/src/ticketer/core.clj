(ns ticketer.core
  "Core module for the ticket tools."
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]
            [java-time :as time]
            [org.httpkit.encode :refer [base64-encode]]
            [clojure.string :as str]))


;; Configurations.
(def *cfg (atom {}))
(def default-cfg {:owner "syl20bnr" :repo "spacemacs" :retry-after-sec 10})


;;;; GitHub API
(def api-base "https://api.github.com/repos")
(def api-status-ok "200 OK")
(def api-status-not-found "404 Not Found")
(def no-retry-statuses #{api-status-ok api-status-not-found})


;;;; Helpers
(def read-file (comp read-string slurp))


(defn read-secret-file
  "Reads and validate file that stores credentials."
  [secfet-file]
  (try
    (let [{:keys [user token]} (read-file secfet-file)]
      (if (and user token)
        (str user ":" token)
        (throw  (Error. "Can't read creds - this message won't be seen."))))
    (catch Exception _
      (->> ["Something went wrong with reading \"secret.edn\""
            "But I won't tell you what!"
            "The file must have the shape:"
            " {:user \"<USER>\" :token \"<GITHUB_API_TOKEN>\"}"]
           (str/join "\n")
           Exception.
           throw))))


(defn configure!
  "Configure application."
  [cfg-file secret-file]
  (reset! *cfg (merge default-cfg
                      (read-file cfg-file)
                      {:auth (read-secret-file secret-file)})))


(defn ms-till-ratelimit-reset
  "How many milliseconds left until rate limit resets."
  [reset-date-hdr]
  (- (* 1000 (Integer/parseInt reset-date-hdr))
     (time/to-millis-from-epoch (time/instant))))


;;;; Actual stuff
(defn gh-request
  "GitHub specific Wrapper for `client/request`.
  The function supplies credentials and automatically retries requests
  unless the response code is a member of `no-retry-statuses`."
  [args]
  (if-not (str/starts-with? (:url args) api-base)
    (throw (java.lang.SecurityException.
            (format "The request URL doesn't start with \"%s\". %s"
                    api-base
                    "The request was blocked to prevent API token leak.")))
    (future
      (loop []
        (let [resp @(client/request (assoc args :basic-auth (:auth @*cfg)))
              {{:keys [status x-ratelimit-remaining x-ratelimit-reset]}
               :headers} resp]
          (if (no-retry-statuses status)
            resp
            (do (Thread/sleep
                 (if (and x-ratelimit-remaining x-ratelimit-reset
                          (< (Integer/parseInt x-ratelimit-remaining) 1))
                   (let [ms-to-wait (->> x-ratelimit-reset
                                         ms-till-ratelimit-reset
                                         ;; Never wait longer than a minute.
                                         ;; Just in case if local clock
                                         ;; is messed up.
                                         (Integer/min 60000))]
                     (prn "Rate limit depleted. Waiting %s seconds"
                          (quot ms-to-wait 1000))
                     ms-to-wait)
                   (let [del (@*cfg :retry-after-sec)]
                     (prn "Bad status \"%s\" retrying in %s sec" status del)
                     (* 1000 del))))
                (recur))))))))


(defn issue-pages
  "Return lazy seq of issues by LABEL."
  [{:keys [labels state sort sort-dir]
    :or {labels [] state "open" sort "created" sort-dir "asc"}}]
  (let [base-url (format "%s/%s/%s/issues"
                         api-base
                         (@*cfg :owner)
                         (@*cfg :repo))
        fetch-first-page (fn []
                           (gh-request
                            {:url (format (str "%s?labels=%s&state=%s"
                                               "&sort=%s&direction=%s"
                                               "&page=%s&per_page=100")
                                          base-url
                                          (str/join "," labels)
                                          state
                                          sort
                                          sort-dir
                                          1)
                             :method :get}))
        page->next-page-url (fn [{{links :link} :headers}]
                              (->> links
                                   (re-find #"<([^>]+)>; rel=\"next\"")
                                   second))]
    ((fn inner [page]
       (lazy-seq (cons page
                       (some->> page
                                page->next-page-url
                                (assoc {:method :get} :url)
                                gh-request
                                deref
                                inner))))
     @(fetch-first-page))))


(defn page->issues
  "Extract issues from PAGE of issues."
  [page]
  (some-> page :body json/parse-string))


(defn issues
  "Converts seq of issue pages into lazy seq of issues."
  [pages]
  (some->> pages
           (map page->issues)
           (filter seq)
           seq
           ((fn inner [[[first-issue & rest-in-chunk] :as iss]]
              (lazy-seq (cons first-issue
                              (some-> (lazy-cat (some-> rest-in-chunk seq list)
                                                (rest iss))
                                      seq
                                      inner)))))))


(defn issue->last-comment
  "Returns last comment of the issue."
  [{url "comments_url"}]
  (let [paged-url (format "%s?per_page=1" url)
        last-page-url (or (some->> paged-url
                                   (assoc {:method :head} :url)
                                   gh-request
                                   deref
                                   :headers
                                   :link
                                   (re-find #"<([^>]+)>; rel=\"last\"")
                                   second)
                          paged-url)]
    (->> last-page-url
         (assoc {:method :get} :url)
         gh-request
         deref
         :body
         json/parse-string
         last)))


(defn comment->user-login
  "Returns user login of the comment"
  [comment]
  (get-in comment ["user" "login"]))


(defn label-remove
  "Removes LABEL from ISSUE.
  FIXME: Looks like GH API uses CGI escape for label names.
         It doesn't escape characters like \"[\" but `client/request`
         wants them escaped. But if you `client/url-encode` them
         then GH API won't recognize the label name..."
  [label {url "labels_url" :as issue}]
  (io! @(gh-request {:url (str/replace url "{/name}" (str "/" label))
                     :method :delete})))


(defn label-add
  "Adds LABEL to ISSUE"
  [label {url "labels_url" :as issue}]
  (io! @(gh-request {:url (str/replace url "{/name}" "")
                     :headers {"Content-Type" "application/json"}
                     :body (format "{\"labels\" : [\"%s\"]}" label)
                     :method :post})))

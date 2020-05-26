(ns ticketer.run
  "Entry point module for the app."
  (:require [ticketer.core :as t]))


(def configs-file "cfg.edn")
(def secrets-file "secret.edn") ;; {:user "<login>" :token "<personal_token>"}
(def log-file "log.edn")
(def label-stale "stale")
(def label-stale-updated "updated")
(def actions-bot-login "github-actions[bot]")


(defn last-commented-by-action-bot?
  "Returns true if the last comment in the issue
  belongs to `actions-bot-login`."
  [issue]
  (some->> issue
           t/issue->last-comment
           t/comment->user-login
           (= actions-bot-login)))


(defn log
  "Logs (FORMATTER X) using `>tap` and returns X."
  [formatter x]
  (tap> (formatter x))
  x)


(defn -main []
  (t/configure! configs-file secrets-file)
  (add-tap println)
  (doall
   (->> {:labels [label-stale]}
        t/issue-pages
        t/issues
        (map (partial log #(format "Checking \"%s\" %s"
                                   (% "title")
                                   (% "url"))))
        (filter (complement last-commented-by-action-bot?))
        (map (partial log #(format "Found updated issue: \"%s\" %s"
                                   (% "title")
                                   (% "url"))))
        (map #(t/label-add label-stale-updated %))
        (pmap (partial log #(format "Labeled \"%s\" with response code: %s"
                                   (get-in % [:opts :url])
                                   (:status %)))))))

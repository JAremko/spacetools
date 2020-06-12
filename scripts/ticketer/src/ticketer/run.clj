(ns ticketer.run
  "Entry point module.
  The app marks stale issues with updated tag if
  the last comment in the issue isn't from the stale bot.
  The app take no arguments and should be configured with
  cfg.edn and secret.edn files in the run folder.

  cfg.edn example:
  {:owner \"syl20bnr\" :repo \"spacemacs\" :retry-after-sec 10}

  secret.edn example:
  {:user \"jaremko\" :token \"<GH API TOKEN\"}
  "
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
        (map (partial log #(format "Checking \"%s\" %s" (% "title") (% "url"))))
        (filter (complement last-commented-by-action-bot?))
        (map (partial log #(format "Found updated issue: \"%s\" %s"
                                   (% "title")
                                   (% "url"))))
        (map #(t/label-add label-stale-updated %))
        (pmap (partial log #(format "Labeled \"%s\" with response code: %s"
                                    (get-in % [:opts :url])
                                    (:status %)))))))

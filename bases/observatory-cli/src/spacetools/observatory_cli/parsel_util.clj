(ns spacetools.observatory-cli.parsel-util
  "Elisp parser helpers."
  (:require [instaparse.combinators :as c]
            [clojure.spec.alpha :as s]))


(defmacro defrule [name doc-string rule]
  `(def ~name
     ~doc-string
     ~(if (string? rule)
        ;; `c/ebnf` blows up if evaluated during macroexpand.
        `(c/ebnf ~rule)
        rule)))

(s/def ::combinator map?)

(s/fdef defrule
  :args (s/cat :name symbol?
               :doc-string string?
               :rule (s/or :string string?
                           :combinator ::combinator))
  :ret list?)

(ns spacetools.spacedoc.node.val-spec
  "Node value specs."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))


;; NOTE: Some lines may be empty but not all of them.
(s/def ::non-blank-lines
  (s/with-gen (s/and string? (complement str/blank?))
    #(gen/fmap str
               (gen/tuple
                (gen/string-alphanumeric)
                (gen/elements ["\n" ""])
                (gen/string-alphanumeric
                 (gen/elements ["\n" ""])
                 (gen/string-alphanumeric))))))


(s/def ::non-blank-string
  (s/and string? (complement str/blank?)))


(s/def ::set-of-non-blank-strings (s/coll-of ::non-blank-string :kind set?))

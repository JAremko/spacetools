(ns spacetools.spacedoc.org.layers
  "layers.org generator."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [orchestra.core :refer [defn-spec]]
            [spacetools.fs-io.interface :as io]
            [spacetools.spacedoc.config :as cfg]
            [spacetools.spacedoc.node :as n]
            [spacetools.spacedoc.util :as sdu]))


(defn-spec root->description (s/nilable :spacetools.spacedoc.node/headline)
  [node :spacetools.spacedoc.node/root]
  (->> node
       :children
       (filter (partial s/valid? :spacetools.spacedoc.node.meta/description))
       first))


(defn-spec describe :spacetools.spacedoc.node/headline
  [node :spacetools.spacedoc.node/root]
  (apply
   n/headline
   (:title node)
   (into (-> (if-let [src (:source node)]
               (n/link src (n/text src))
               (n/text "<layer link is missing>"))
             n/paragraph
             (n/section (n/paragraph (n/line-break)))
             vector)
         (:children
          (sdu/flatten-headline 1 (root->description node))
          (->> "README.org of the layer misses or has invalid \"Description\"."
               n/text
               n/bold
               n/paragraph
               n/section
               (n/headline "placeholder"))))))


;; TODO Replace TAG validation with SPEC on configs read.
;; TODO Handle case when the root node ends up empty.
(defn-spec layers-sdn (s/nilable :spacetools.spacedoc.node/root)
  "Create root node for layers.org(SDN) fixing relative paths in descriptions.
ROOT-DIR is the directory that will be used to resolve relative paths against.
In PATH->SDN map PATH(keys) are original file paths and SDN(values) are docs."
  [root-dir io/file-ref?
   path->sdn (s/map-of io/file-ref? :spacetools.spacedoc.node/root)]
  (letfn [(walk [ds node]
            (let [tag (if (map? node)
                        (ffirst node)
                        node)]
              (if-not ((cfg/valid-tags) tag)
                (throw (ex-info "Query has invalid tag" {:tag tag}))
                (when-let [f-ds (seq (filter #((:tags %) tag) ds))]
                  (apply n/headline (or ((cfg/valid-tags) tag)
                                        (->> tag
                                             (format "<\"%s\" invalid tag>")
                                             str/upper-case))
                         (if (string? node)
                           (mapv #(describe %) f-ds)
                           (->> node
                                first
                                val
                                (map (partial walk f-ds))
                                (remove nil?))))))))

          (rm-file-pref [path]
            (str/replace path #"^file:" ""))

          (add-file-pref [path]
            (str "file:" path))

          (relativize [path old-root other]
           (io/relativize path (io/join (io/parent old-root) other)))

          (re-root-sdn [path sdn]
            (assoc sdn
                   :source
                   (-> root-dir
                       (relativize path (rm-file-pref path))
                       (str/replace #"(?ix)\.sdn$" ".org")
                       add-file-pref)
                   :root-dir root-dir))

          (fix-relative-links [path sdn]
            (apply
             update sdn
             (if (s/valid? :spacetools.spacedoc.node/link sdn)
               [:path #(if (= (:type sdn) :file)
                         (->> %
                              rm-file-pref
                              (relativize root-dir path)
                              add-file-pref)
                         %)]
               [:children (partial mapv (partial fix-relative-links path))])))]

    (some->> (cfg/layers-org-query)
             seq
             (hash-map "layer")
             (walk (pmap #(->> % (apply re-root-sdn)
                               (fix-relative-links (first %)))
                         path->sdn))
             :children
             seq
             (apply n/root "Configuration layers" #{}))))

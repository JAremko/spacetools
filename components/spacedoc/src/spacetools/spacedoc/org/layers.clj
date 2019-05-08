(ns spacetools.spacedoc.org.layers
  "layers.org generator."
  (:require [clojure.set :refer [difference]]
            [clojure.spec.alpha :as s]
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
               (n/link (str "file:" src) (n/text src))
               (n/text "<layer link is missing>"))
             n/paragraph
             (n/section (n/paragraph (n/line-break)))
             vector)
         (if-let [description (root->description node)]
           (:children (sdu/flatten-headline 1 description))
           (->> "README.org of the layer misses or has invalid \"Description\"."
                n/text
                n/bold
                n/paragraph
                n/section
                (n/headline "placeholder")
                vector)))))


(defn rm-file-prefix
  [path]
  (str/replace path #"^file:" ""))


(defn add-file-prefix
  [path]
  (str "file:" path))


(defn relativize
  [path old-root other]
  (io/relativize path (io/join (io/parent old-root) other)))


(defn re-root-sdn
  [root-dir path sdn]
  (assoc sdn
         :source
         (-> root-dir
             (relativize path (rm-file-prefix path))
             (str/replace #"(?ix)\.sdn$" ".org"))
         :root-dir root-dir))


(defn fix-relative-links
  [root-dir path doc]
  ((fn inner [f-p sdn]
     (if (s/valid? :spacetools.spacedoc.node/link sdn)
       (condp = (:type sdn)
         :file (update sdn :path #(->> %
                                       rm-file-prefix
                                       (relativize root-dir f-p)
                                       add-file-prefix))
         :custom-id  (assoc sdn
                            :path (add-file-prefix (:source doc))
                            :type :file)
         sdn)
       (update sdn :children (partial mapv (partial inner path)))))
   path doc))


;; TODO Replace TAG validation with SPEC on configs read.
;; TODO Handle case when the root node ends up empty.
(defn-spec layers-sdn (s/nilable :spacetools.spacedoc.node/root)
  "Create root node for layers.org(SDN) fixing relative paths in descriptions.
ROOT-DIR is the directory that will be used to resolve relative paths against.
In PATH->SDN map PATH(keys) are original file paths and SDN(values) are docs."
  [root-dir io/file-ref?
   path->sdn (s/map-of io/file-ref? :spacetools.spacedoc.node/root)]
  (let [all-docs-v (volatile! #{})
        walk (fn inner [ds node]
               (let [tag (if (map? node)
                           (ffirst node)
                           node)]
                 (if-not ((cfg/valid-tags) tag)
                   (throw (ex-info "Query has invalid tag" {:tag tag}))
                   (when-let [f-ds (seq (filter #(and ((:tags %) tag)
                                                      (@all-docs-v %))
                                                ds))]
                     (apply n/headline (or ((cfg/valid-tags) tag)
                                           (->> tag
                                                (format "<\"%s\" invalid tag>")
                                                str/upper-case))
                            (if (map? node)
                              (->> node
                                   first
                                   val
                                   (map (partial inner f-ds))
                                   (remove nil?))
                              (do (vswap! all-docs-v difference f-ds)
                                  (->> f-ds
                                       (sort-by :title)
                                       (mapv describe)))))))))]

    (apply n/root "Configuration layers" #{}
           (or (seq
                (concat (->> (cfg/layers-org-query)
                             (hash-map "layer")
                             (walk (->> path->sdn
                                        (pmap
                                         #(->> %
                                               (apply re-root-sdn root-dir)
                                               (fix-relative-links root-dir
                                                                   (first %))))
                                        set
                                        (vreset! all-docs-v)))
                             :children)
                        (some->> @all-docs-v
                                 (filter #(contains? (:tags %) "layer"))
                                 seq
                                 (sort-by :title)
                                 (map describe)
                                 (apply n/headline "Skipped layers:"))))
               (n/todo "No README.org files tagged with \"layer\" tag")))))

(ns spacetools.observatory-cli.run
  (:require [clojure.core.reducers :as r]
            [spacetools.observatory-cli.elisp.core :as el]
            [spacetools.observatory-cli.elisp.keybinding :as kb]))


(defn -main [dir target-f & args]
  (printf "Dumping legacy bindings from dir \"%s\" to \"%s\" file...\n"
          dir target-f)
  (->> dir
       clojure.java.io/file
       file-seq
       (keep (comp (partial re-matches #".*\.el$") str))
       (r/fold (r/monoid #(->> %2
                               slurp
                               el/read-str
                               kb/collect-legacy-bindings
                               (hash-map :file %2 :legacy-bindings)
                               (conj %1))
                         vector))
       (remove (complement (comp seq :legacy-bindings)))
       (hash-map :tag :legacy-bindings-report :content)
       (spit target-f))
  (prn "Done!"))

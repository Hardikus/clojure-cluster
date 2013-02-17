;; Clustering algorithms in Clojure
;;
;; Perform a hierarchical cluster with: hcluster
;;   (hcluster vectors)
;;   vectors - a sequence of vectors
;;
;; Perform k-means clustering with: kcluster
;;   (kcluster vectors number-of-clusters range-start range-end)
;;   vectors - a sequence of vectors
;;   

(ns cluster.core
    (:use cluster.internal)
    (:use cluster.clustering)
    )

;; TODO: deprecated
(comment
(defn kcluster
  "Performs k-means clustering.
    :vectors - a sequence of vectors
    :how-many - how many clusters to find
    :start - lower limit of numbers in vectors
    :end - upper limit of numbers in vectors

  (kcluster [[1 2] [3 4] [5 6]] 2 0 6)"
  ([vectors how-many start end]
     (kcluster vectors (random-vectors how-many (count (nth vectors 0)) start end)))
  ([vectors nodes]
    (k-means1 vectors nodes)))
)

(defn hcluster 
  "Performs hierarchical clustering.
    :nodes - a sequence of maps of the form:
      { :vec [1 2 3] }
   The return value will be a tree of Maps of the form:
      { :vec [] :left { :vec ... } :right { :vec ... } }"
  [nodes]
  (if (< (count nodes) 2)
    nodes
    (let [vectors (vec (map :vec nodes))
          [l r :as closest-pair] (closest-vectors vectors)
          new-nodes (vec (for [i (range (count nodes)) :when (and (not= i l) (not= i r))] (nodes i)))]
      (hcluster (conj 
                 new-nodes
                 {:left (nodes l)
                  :right (nodes r)
                  :vec (centroid
                         (:vec (nodes l) ) 
                         (:vec (nodes r) ))})))))



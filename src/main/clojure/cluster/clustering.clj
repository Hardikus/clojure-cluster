(ns cluster.clustering
  (:use [cluster.internal]))

(defn min-distance [v means] 
  (apply 
    min-key 
    #(euclidean-distance v %) 
    means))

(defn forgy-init [xs k]
  (let [xs (if (map? xs) (vals xs) xs)]
    (take k (shuffle xs))))

(defn random-partition [xs k]
  (let [xs (if (map? xs) (vals xs) xs)]
    (map centroid (partition k (shuffle xs)))))


(defn- k-means1
  ([vectors init-method k]
    (k-means1 vectors (init-method vectors k)))
  ([vectors means]
    (let [k (count means)
;          vectors (vec vectors)
          ]
      (loop [means means]
        (let [clusters (group-by 
                         #(min-distance (vectors %) means)
                         (if (vector? vectors)
                           (range (count vectors))
                           (keys vectors)))
              nmeans (map #(apply centroid (map vectors %)) (vals clusters))]
          (if (= means nmeans)
            (vals clusters)
            (recur nmeans)))))))


(defn k-means 
  ([vectors k]
    (k-means1 vectors forgy-init k))
  ([vectors k init-method]
    (k-means1 vectors init-method k)))


;;-----
(defn gen-vec [m length] 
;  (sort-by first 
;           (reduce conj [] m)))
(reduce 
  (fn [v [key val]] (if key (assoc v key val) v)) 
  (into [] (repeat length 0))
  m))

(defn group-count [f g coll]
  (let [groups (group-by g coll)]
    (apply array-map
      (interleave
        (keys groups)
        (for [[_ g] groups]
          (frequencies
            (map f g)))))))

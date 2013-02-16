(ns cluster.clustering)

;(defn random-vector [length range-start range-end]
;  (let [diff (- range-end range-start)]
;    (repeatedly length 
;      #(+ range-start (rand diff)))))

(defn sqr [x]
  (* x x))


(defn vector-add [& xs]
  (apply map + xs))
(defn vector-substract [& xs]
  (apply map - xs))
(defn vector-scalar-product [v x]
  (map #(* x %) v))
(defn vector-norm [v]
  (Math/sqrt (reduce + (map sqr v))))
(defn vector-euclidean-distance [x y]
  (Math/sqrt (reduce + (map sqr (vector-substract x y)))))

(defn sparse-add [& vs]
  (apply merge-with + vs))
(defn sparse-substract [& vs]
  (apply merge-with - vs))
(defn sparse-norm [v] 
  (Math/sqrt (reduce + (map sqr (vals v)))))
(defn sparse-scalar-product [v x] 
  (reduce 
    (fn [m k] (update-in m [k] * x)) 
    v 
    (keys v)))
(defn sparse-euclidean-distance [v w] 
  (Math/sqrt (reduce + (map sqr (vals (sparse-substract v w))))))

(defprotocol Vector
  (add [v w])
  (substract [v w])
  (norm [v])
  (scalar-product [v x])
  (euclidean-distance [v w]))

(extend clojure.lang.PersistentVector
  Vector
  {:add (fn [v w] (into [] (vector-add v w)))
   :substract (fn [v w] (into [] (vector-substract v w)))
   :norm vector-norm
   :scalar-product vector-scalar-product
   :euclidean-distance vector-euclidean-distance})

(extend clojure.lang.APersistentMap
  Vector
  {:add (fn [v & vs] (apply sparse-add v vs))
   :substract (fn [v & vs] (apply sparse-substract v vs))
   :norm sparse-norm
   :scalar-product sparse-scalar-product
   :euclidean-distance sparse-euclidean-distance})

(defn centroid [& xs]
  (scalar-product
    (reduce add xs)
    (double (/ 1 (count xs)))))


(defn sparse-vector [coll]
  (into 
    {} (filter
         (fn [[_ v]] (or (> v 0) (< v 0)))
         (map-indexed vector coll))))

(defn min-distance [v means] 
  (apply 
    min-key 
    #(euclidean-distance v %) 
    means))

(defn forgy-init [xs k]
  (take k (shuffle xs)))

(defn random-partition [xs k]
  (map centroid (partition k (shuffle xs))))

(defn k-means1
  ([vectors init-method k]
    (k-means1 vectors (init-method vectors k)))
  ([vectors means]
    (let [k (count means)
          vectors (vec vectors)]
      (loop [means means]
        (let [clusters (group-by 
                         #(min-distance (nth vectors %) means)
                         (range (count vectors)))
              nmeans (map #(apply centroid (map (partial nth vectors) %)) (vals clusters))]
          (if (= means nmeans)
            (vals clusters)
            (recur nmeans)))))))


(defn k-means [vectors k]
  (k-means1 vectors forgy-init k))


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

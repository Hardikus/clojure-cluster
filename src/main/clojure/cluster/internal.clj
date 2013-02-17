;; Internal functions used by clojure.cluster

(ns cluster.internal
  (:use [clojure.contrib.generic.math-functions :only [sqr sqrt]]
        [incanter.stats :only [variance covariance mean]]))

(defn- sum-of-squares [coll]
  (reduce (fn [^double x ^double y] (+ x y)) 
          (map (fn [^double x ^double y] (* x y)) coll coll)))

(defprotocol Vector
  (each [v op w] "Yields a new vector constructed by applying op to each pair (vi, wi) in v and w,
if w is a vector or each pair (vi, w) if w is a number.")
  (norm [v]))

(extend-type clojure.lang.PersistentVector
  Vector
  (each [v op w]
    (vec (cond 
           (number? w) 
           (map op v (repeat w))
           
           (and (vector? w) (>= (count v) (count w)))
           (map op v (lazy-cat w (repeat 0)))
           
           :default
           (map op (lazy-cat v (repeat 0)) w))))
  (norm [v] (sqrt (sum-of-squares v))))



(extend-type clojure.lang.APersistentMap
  Vector
  (each [v op w] 
    (let [w (if (number? w) (zipmap (keys v) (repeat w)) w)]
      (merge-with op v w)))
  (norm [v] (sqrt (sum-of-squares (vals v)))))

(defn centroid [& xs]
  (each
    (reduce (fn [v w] (each v + w)) xs)
    *
    (double (/ 1 (count xs)))))

(defn euclidean-distance [v w] 
  (norm (each v - w)))

(defn sparse-vector [coll]
  (into 
    {} (filter
         (fn [[_ v]] (or (> v 0) (< v 0)))
         (map-indexed vector coll))))

;; TODO: pearson has to be refactored, does not work on maps
(defn pearson [x y]
  (/ (covariance x y)
     (sqrt (* (variance x)
              (variance y)))))


(defn closest-vectors [vs]
  (apply min-key
    (fn [[x y]] (euclidean-distance (vs x) (vs y)))
    (for [i (range (count vs))
          j (range (inc i) (count vs))]
      [i j])))





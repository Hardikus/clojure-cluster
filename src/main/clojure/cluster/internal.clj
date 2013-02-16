;; Internal functions used by clojure.cluster

(ns cluster.internal
  (:use [clojure.contrib.generic.math-functions :only [sqr sqrt]]
        [incanter.stats :only [variance covariance mean]]))

(defmacro sum [v] `(reduce + ~v))
;
;(defn variance [v] 
;  (sum 
;    (for [vi v] 
;      (sqr 
;        (- vi (mean v))))))
;
;(defn covariance [v w] 
;  (sum 
;    (for [i (range (count v))] 
;      (sqr 
;        (* (- (v i) (mean v)) (- (w i) (mean w)))))))

;(defn std-dev [v] (let [meanv ]
;                    (/ (variance v)
;                       (count v))))
; for Bessel's correction:
;                       (dec (count v)))))


(defn pearson [x y]
  (/ (covariance x y)
     (sqrt (* (variance x)
              (variance y)))))
     
;  (let [n (count x)
;        sum-y (sum y)
;        sum-x (sum x)
;        prod-of-sqrts (* (sqrt (- (sum (map sqr x))
;                                  (/ (sqr sum-x) n))) 
;                         (sqrt (- (sum (map #(square %) y))
;                                  (/ (sqr sum-y) n))))]
;    (if (= 0.0 prod-of-sqrts)
;      nil
;      (/ (- (sum (map #(* %1 %2) x y))
;            (/ (* sum-x sum-y) n)) 
;         prod-of-sqrts))))



(defn compact [v]
  (filter #(not (nil? %)) v))
    

(defn average-vectors [vectors]
  (loop [rests vectors out '()]
    (let [firsts (compact (map first rests))]
      (if (= 0 (count rests))
        out
        (recur (compact (map rest rests)) `(~@out ~(mean firsts)))))))

(defmacro grtr [n1 n2] `(if (> ~n1 ~n2) ~n1 ~n2))

(defn closest-vector
  ([target others] (closest-vector target others 0))
  ([target others n]
   (let [current (first others)
         sim (or (pearson target current) 0.0)
         others (rest others)]
     (if (= 0 (count others))
       [sim, n]
       (let [[other-sim, other-n] (closest-vector target others (inc n))]
         (if (> sim other-sim) [sim,n] [other-sim,other-n]))))))
      
(defn n-in-cycle [cyclen start i]
  (let [absi (+ start i 1)]
    (if (>= absi cyclen)
      (n-in-cycle cyclen start (- absi cyclen))
      absi)))

(defn closest-vectors [vs]
  (let [vcyc (cycle vs) vlen (count vs)]
    (loop [ptr 0 pair [0 0] c -1.0]
      (if (= ptr vlen)
        [pair c]
        (let [[cp cn] (closest-vector 
                       (nth vs ptr) 
                       (take (dec vlen) (nthrest vcyc (inc ptr))))
              closer? (if (> cp c) true nil)]
          (recur (inc ptr)
                 (if closer? [ptr (n-in-cycle vlen ptr cn)] pair)
                 (if closer? cp c)))))))

(defn include? [v i]
  (some #(= i %) v))

(defn without [v & more]
  (loop [cv v ov '() ptr 0]
    (if (= 0 (count cv))
      ov
      (recur (rest cv)
             (if (not (include? more ptr))
               `(~@ov ~(first cv))
               ov)
             (inc ptr)))))


(defn random-vector [length range-start range-end]
  (into [] 
        (for [index (range length)]
          (+ range-start 
             (rand (- range-end range-start))))))

(defn random-vectors [k length range-start range-end]
  (for [i (range k)]
    (random-vector length range-start range-end)))





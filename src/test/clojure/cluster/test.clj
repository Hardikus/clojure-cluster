;; Tests for clojure.cluster

(ns cluster.test
    (:use clojure.test)
    (:use cluster.clustering)
;    (:use cluster.core)
;    (:use cluster.internal)
    )


;; hcluster tests

(deftest test-hcluster-empty-nodes []
  (is (= [] (hcluster []))))

(deftest test-hcluster-single-node []
  (is (= [{:vec []}]
         (hcluster [{:vec []}]))))
  
(deftest test-hcluster-two-nodes []
  (is (= '({:left {:vec [1 2]}
            :right {:vec [2 3]}
            :vec [3/2 5/2]})
         (hcluster [{:vec [1 2]}
                    {:vec [2 3]}]))))

(deftest test-hcluster-three-nodes []
  (is (= '({:left {:left {:vec [1 2]} 
                   :right {:vec [4 8]}
                   :vec (5/2 5)}
            :right {:vec [2 3]}
            :vec (9/4 4)})
         (hcluster [{:vec [1 2]}
                    {:vec [2 3]}
                    {:vec [4 8]}]))))


;; kcluster tests

(deftest test-kcluster-two-nodes []
  (let [[[[x] [y]] nodes] (kcluster [[1 2] [2 1]] 2 1 2)]
    (is (and (or (= x 1) (= x 0))
             (or (= y 1) (= y 0))
             (= (+ x y) 1)))))


;; internal tests
    
(deftest test-mean []
  (is (= (mean [0]) 0))
  (is (= (mean [1]) 1))
  (is (= (mean [1 2]) 1.5)))

(deftest test-sqrt []
  (is (= (sqrt 4) 2))
  (is (= (sqrt 9) 3))
  (is (= (sqrt 2.25) 1.5)))

(deftest test-square []
  (is (= (square 3) 9))
  (is (= (square 0) 0)))

(deftest test-sum []
  (is (= (sum [1 2 3]) 6)))

(deftest test-pearson []
  (is (= 0.9999999999999998 (pearson [1 2] [1 2])))
  (is (= -0.9999999999999998 (pearson [1 2] [2 1])))
  (is (= 0.0 (pearson [1 2 3] [1 2 1]))))
;
;(deftest test-compact []
;  (is (= [1 2 3] (compact [nil 1 nil 2 nil 3 nil nil])))
;
;(deftest test-average-vectors []
;  (is (= [2 2 2] (average-vectors [[1 3 1] [3 1 3] [2 2 2]])))
;
;(deftest test-grtr []
;  (is (= 2 (grtr 1 2))
;  (is (= 2 (grtr 2 1)))
;
;(deftest test-closest-vector []
;  (is (= [0.9999999999999998, 0] (closest-vector [1 2 3] [[1 2 3] [1 2 1]]))
;  (is (= [0.9999999999999998, 1] (closest-vector [1 2 3] [[1 2 1] [1 2 3]]))
;  (is (= [0.0, 1] (closest-vector [1 2 3] [[3 2 1] [1 2 1]])))
;
;(deftest test-closest-vectors []
;  (is (= [[0 1] 0.9999999999999998] (closest-vectors [[1 2 3] [1 2 3] [1 2 1]]))
;  (is (= [[0 2] 0.9999999999999998] (closest-vectors [[1 2 3] [1 2 1] [1 2 3]]))
;  (is (= [[1 2] 0.9999999999999998] (closest-vectors [[1 2 1] [1 2 3] [1 2 3]])))
;
;(deftest test-include? []
;  (assert-true (include? [1 2 3] 1))
;  (assert-nil (include? [1 2 3] 4)))
;
;(deftest test-without []
;  (is (= [1 2 4] (without [1 2 3 4] 2))
;  (is (= [1 4] (without [1 2 3 4] 2 1)))
;
;(deftest test-random-vector []
;  (let [r (random-vector 5 0 1)]
;    (is (= 5 (count r))
;    (is (= true (every? #(< % 1) r))
;    (is (= true (every? #(> % 0) r))))
;
;(deftest test-random-vectors []
;  (let [rs (random-vectors 4 5 0 1)]
;    (is (= 4 (count rs))
;    (assert-true (every? #(= 5 (count %)) rs))))

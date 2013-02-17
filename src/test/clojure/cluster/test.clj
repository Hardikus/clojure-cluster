;; Tests for clojure.cluster

(ns cluster.test
    (:use clojure.test)
    (:use cluster.clustering)
    (:use cluster.core)
    (:use cluster.internal)
    (:use [clojure.contrib.generic.math-functions :only [approx=]])
    )


(deftest test-hcluster-empty-nodes []
  (is (= [] (hcluster []))))

(deftest test-hcluster-single-node []
  (is (= [{:vec []}]
         (hcluster [{:vec []}]))))
  
(deftest test-hcluster-two-nodes []
  (is (= '({:left {:vec [1 2]}
            :right {:vec [2 3]}
            :vec [1.5 2.5]})
         (hcluster [{:vec [1 2]}
                    {:vec [2 3]}]))))

(deftest test-hcluster-three-nodes 
  (let [cluster (hcluster [{:vec [1 2]}
                           {:vec [2 3]}
                           {:vec [4 8]}])
        [{cent1 :vec :as node1}] cluster
        {{cent2 :vec} :left} node1
        {{cent3 :vec} :right} node1]
    (is (= [2.75 5.25] cent1))
    (is (= [4 8] cent2))
    (is (= [1.5 2.5] cent3))))

(deftest test-hcluster-map-three-nodes 
  (let [cluster (hcluster [{:vec {:a 1 :b 2}}
                           {:vec {:a 2 :b 3}}
                           {:vec {:a 4 :b 8}}])
        [{cent1 :vec :as node1}] cluster
        {{cent2 :vec} :left} node1
        {{cent3 :vec} :right} node1]
    (is (= {:a 2.75 :b 5.25} cent1))
    (is (= {:a 4 :b 8} cent2))
    (is (= {:a 1.5 :b 2.5} cent3))))


;; kcluster tests

(deftest test-k-means-two-nodes
  (let [[[c1] :as clusters] (k-means [[1 2] [2 1]] 1)]
    (is (= 1 (count clusters)))
    (is (= c1 0))))

(deftest test-k-means-three-nodes
  (let [[[c1] [c2] :as clusters] 
        (k-means [[1 2] [2 3] [4 8]] 2)]
    (is (= 2 (count clusters)))
    (is (= c1 0))
    (is (= c2 2))))


;; internal tests

(deftest test-pearson 
  (is (== 1.0 (pearson [1 2] [1 2])))
  (is (== -1.0 (pearson [1 2] [2 1])))
  (is (== 0.0 (pearson [1 2 3] [1 2 1])))
  (is (== 1.0 (pearson [1 2 3 5 8] [0.11, 0.12, 0.13, 0.15, 0.18]))))


(deftest test-euclidean-distance 
  (is (== 0.0 (euclidean-distance [1 2] [1 2])))
  (is (approx= 1.414 (euclidean-distance [1 2] [2 1]) 0.001))
  (is (== 3.0 (euclidean-distance [4,1,-2] [2,3,-1])))
  (is (== 2.0 (euclidean-distance [1 2 3] [1 2 1])))
  (is (approx= 9.860
          (euclidean-distance [1 2 3 5 8] [0.11, 0.12, 0.13, 0.15, 0.18])
          0.001)))

(deftest test-centroid
  (is (= [2.0 2.0 2.0] (centroid [1 3 1] [3 1 3] [2 2 2])))
  (is (= 3 (count (centroid [1 3 1] [3 1 3] [2 2]))))
  (is (= 3 (count (centroid [1 3] [3 1 3] [2 2 2]))))
  (is (= {:a 2.0 :b 2.0 :c 2.0} (centroid {:a 1 :b 3 :c 1} {:a 3 :b 1 :c 3} {:a 2 :b 2 :c 2}))))

(deftest test-min-distance
  (is (= [1 2 3] (min-distance [1 2 3] [[1 2 3] [1 2 1]])))
  (is (= [1 2 3] (min-distance [1 2 3] [[1 2 1] [1 2 3]])))
  (is (= [1 2 1] (min-distance [1 2 3] [[3 2 1] [1 2 1]]))))

;; WARN: pearson correlation is at max when the vectors are similar
;; whereas other similarity measures like euclidian distance are at min
(deftest test-closest-vectors []
  (is (= [0 1] (closest-vectors [[1 2 3] [1 2 3] [1 2 1]])))
  (is (= [0 2] (closest-vectors [[1 2 3] [1 2 1] [1 2 3]])))
  (is (= [1 2] (closest-vectors [[1 2 1] [1 2 3] [1 2 3]]))))


(ns genetics.core-test
  (:require [clojure.test :refer :all]
            [genetics.core :refer :all]))

(deftest fitness-test
  (testing "Fitness function"
    (is (= 2 (fitness [1 1 0] [0 1 0])))))

(deftest fittest-test
  (testing "Fittest function"
    (is (= [1 1 0] (fittest [1 1 0] [[1 1 0] [0 1 0]])))))

(def test-mutation (mutate [1 1 0]))

(deftest mutate-test
  (testing "Mutate function"
    (is (= test-mutation test-mutation))))

(def test-crossover (crossover [1 0 0] [1 1 0]))

(deftest crossover-test
  (testing "Crossover function"
    (is (= test-crossover test-crossover))))

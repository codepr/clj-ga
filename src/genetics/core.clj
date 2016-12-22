;;;; Author Codep
;;; A really simple implementation of a genetic algorithm based on tournament
;;; selection

(ns genetics.core (:gen-class) (:require [clojure.data :refer [diff]]))

;;; Algorithm constants
(def uniform-rate 0.5)
(def mutation-rate 0.015)
(def tournament-size 5)

;;; Population sample constants
(def individual-size 100)
(def population-size 15)


(defn index-of
  "A reliable function to retrieve the index of a element inside a list, a
  vector or a string."
  [item coll]
  (let [v (if (or (vector? coll) (string? coll))
            coll
            (apply vector coll))]
    (.indexOf coll item)))

(defn fitness
  "Calculate the fitness level of an individual"
  [solution individual]
  (count (remove nil? (peek (diff solution individual)))))

(defn fittest
  "Calculate the fittest of the population according to his fitness level"
  [solution population]
  (let [fit-list (map (fn [x] (fitness solution x)) population)]
    (nth population (index-of (apply max fit-list) fit-list))))


(defn crossover
  "Breed a new individual by selecting some gene from two parents"
  [individual1 individual2]
  (map (fn [x y] (if (< (rand) uniform-rate) x y)) individual1 individual2))

(defn mutate
  "Add a mutation in the individual genome, according to a low factor"
  [individual]
  (map (fn [x] (if (< (rand) mutation-rate) (rand-int 2) x)) individual))

(defn tournament-selection
  "Challenging tournament to build a tournament-size population in order to select
  new fitter individuals. Select individuals for crossover, breed new generation"
  [solution population]
  (fittest solution (repeatedly tournament-size #(rand-nth population))))

(defn evolve
  "Evolve the population gradually increasing the fittest individual score"
  [solution population]
  (repeatedly
    (count population)
    #(mutate
       (crossover
         (tournament-selection solution population)
         (tournament-selection solution population)))))

(defn iterate-generations
  "Iterate through generations till the first evolved gene"
  [solution population max-fitness generation]
  (let [current (fitness solution (fittest solution population))]
    (println "Epoch: " generation " fitness: " current)
    (if (< current max-fitness)
      (recur solution (evolve solution population) max-fitness (inc generation)))))

(defn -main
  "Define input data and start iterations"
  [& args]
  (let [s (repeatedly individual-size #(rand-int 2))
        size (* individual-size population-size)
        [p f] [(partition individual-size (repeatedly size #(rand-int 2))) (count s)]]
    (iterate-generations s p f 0)))

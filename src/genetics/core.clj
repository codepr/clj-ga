(ns genetics.core
  (:gen-class)
  (:require [clojure.data :refer [diff]]))

(def uniform-rate 0.5)
(def mutation-rate 0.015)
(def tournament-size 5)

(def individual-size 400)
(def population-size 20)

(defn fitness
  "Calculate the fitness level of an individual"
  [solution individual]
  (count (remove nil? (peek (diff solution individual)))))

(defn index-of
  "Clojure doesn't have an index-of function. The Java .indexOf method
  works reliably for vectors and strings, but not for lists. This solution
  works for all three."
  [item coll]
  (let [v (if (or (vector? coll) (string? coll))
            coll
            (apply vector coll))]
    (.indexOf coll item)))

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

(defn tournament
  "Challenging tournament to build a tournament-size population in order to select
  new fitter individuals"
  [population]
  (repeatedly tournament-size #(rand-nth population)))

(defn tournament-selection
  "Select individuals for crossover, breed new generation"
  [solution population]
  (fittest solution (tournament population)))

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
    (when (< current max-fitness)
      (println "Epoch: " generation " fitness: " current)
      (recur solution (evolve solution population) max-fitness (inc generation)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [s (repeatedly individual-size #(rand-int 2))
        size (* individual-size population-size)
        [p f] [(partition individual-size (repeatedly size #(rand-int 2))) (count s)]]
    (iterate-generations s p f 0)))

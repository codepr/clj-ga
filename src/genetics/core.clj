(ns genetics.core
  (:gen-class))

(use 'clojure.data)

(defn fitness
  "Calculate the fitness level of an individual"
  [solution individual]
  (count (remove nil? (peek (diff solution individual)))))

(defn fittest
  "Calculate the fittest of the individual according to his fitness level"
  [solution population max-fitness best]
  (if (zero? (count population))
    best
    (do
      (def current-max-fitness (fitness solution (first population)))
      (if (>= current-max-fitness max-fitness)
        (recur solution (next population) current-max-fitness (first population))
        (recur solution (next population) max-fitness best)))))

(def s [1 1 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 0 0 1 1 0 0 0 1 1 1 0 1 1 1 0 1])
(def i [1 1 1 0 1 0 0 0 1 1 1 0 1 0 0 0 1 1 1 0 1 0 0 0 1 1 1 0 1 0 0 0 1 1 1 0 1 0 0 0])
(def x [1 1 1 0 1 1 0 1 1 1 1 1 0 0 0 0 1 0 1 1 1 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0])
(def y [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 1 1 0 1 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])
(def z [1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 1 1 0 0 1 0 1 0 1 0 0 0 1 0 1 0 1 0 0 0 1 1 1 0])

(fittest s (list y x z) 0 z)
(fitness s x)

(def uniform-rate 0.5)

(defn crossover
  "Crossover between two individual to breed a new generation"
  [individual1 individual2]
  (if (= 0 (count individual1))
    '()
    (if (< (rand) uniform-rate)
      (cons (first individual1) (crossover (next individual1) (next individual2)))
      (cons (first individual2) (crossover (next individual1) (next individual2))))))

(crossover s i)

(def mutation-rate 0.015)

(defn mutate
  "Mutate a gene according to a low chance"
  [individual]
  (if (zero? (count individual))
    '()
    (if (< (rand) mutation-rate)
      (cons (rand-int 2) (mutate (next individual)))
      (cons (first individual) (mutate (next individual))))))

(mutate s)

(def tournament-size 5)

(defn tournament
  "Select individuals for crossover, breed new generation"
  [population iteration]
  (if (= iteration tournament-size)
    '()
    (cons
     (nth population (rand-int tournament-size))
     (tournament population (inc iteration)))))

(tournament (list x y z x y z x y z y y z y x y z) 0)

(defn tournament-selection
  "Select individuals for crossover, breed new generation"
  [population]
  (fittest s (tournament population 0) 0 (first population)))

(tournament-selection (list x y z x y z x y z y y z y x y z))

(def elitism-offset true)

(defn evolve
  "Evolve generation to get fittest"
  [population iteration]
  (def new-individual
    (mutate
     (crossover
      (tournament-selection population)
      (tournament-selection population))))
  (if (= iteration (count population))
    '()
    (cons new-individual (evolve population (inc iteration)))))

(evolve (list y y z i y z i y z y y z y i y z i z y z z) 0)

(defn iterate-generations
  "Iterate through generations till the first evolved gene"
  [population max-fitness generation]
  (if (< (fitness s (fittest s population 0 (first population))) max-fitness )
    (do
      (println "Generation: " generation " fitness: "(fitness s (fittest s population 0 (first population))))
      (recur (evolve population 0) max-fitness (inc generation)))
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (iterate-generations (list y x z i y z i y z x y z y i y z i x y z z) (fitness s s) 0))

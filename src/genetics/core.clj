(ns genetics.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(use 'clojure.data)

(defn fitness
  "Calculate the fitness level of an individual"
  [solution individual]
  (count (remove nil? (nth (diff solution individual) 2))))

(def s [1 1 1 1 0 1 0 1])
(def i [1 1 1 0 1 0 1 1])

(fitness s i)

(def uniform-rate 0.5)

(defn crossover
  "Crossover between two individual to breed a new generation"
  [individual1 individual2 index]
  (if (= index (count individual1))
    '()
    (if (< (rand) uniform-rate)
      (cons (nth individual1 index) (crossover individual1 individual2 (inc index)))
      (cons (nth individual2 index) (crossover individual1 individual2 (inc index))))))

(crossover s i 0)

(def mutation-rate 0.015)

(defn mutate
  "Mutate a gene according to a low chance"
  [individual index]
  (if (= index (count individual))
    '()
    (if (< (rand) mutation-rate)
      (cons (rand-int 1) (mutate individual (inc index)))
      (cons (nth individual index) (mutate individual (inc index))))))

(mutate s 0)

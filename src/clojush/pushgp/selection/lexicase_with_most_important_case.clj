(ns clojush.pushgp.selection.lexicase-with-most-important-case
  (:use [clojush random simplification]
        clojush.pushgp.selection.lexicase))

(defn lexicase-with-most-important-case-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order.
  Also, adds to the selected individual which case was most important in its
  selection."
  [pop generation argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         last-case (first cases)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (auto-simplify-plush-one-case (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))


(defn lexicase-with-most-important-case-selection-mutate
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order.
  Also, adds to the selected individual which case was most important in its
  selection."
  [pop generation argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         last-case (first cases)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (auto-mutate-plush-one-case (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))


(defn lexicase-with-most-important-case-selection-mutate-more-steps
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order.
  Also, adds to the selected individual which case was most important in its
  selection."
  [pop generation argmap]
  (println "call lexicase more steps")
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         last-case (first cases)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (auto-mutate-plush-one-case-more-steps (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))


(defn lexicase-with-most-important-case-selection-constant-mutate
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order.
  Also, adds to the selected individual which case was most important in its
  selection."
  [pop generation argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         last-case (first cases)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (auto-constant-mutate-plush-one-case (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))



(defn lexicase-with-most-important-case-selection-constant-mutate-more-steps
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order.
  Also, adds to the selected individual which case was most important in its
  selection."
  [pop generation argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         last-case (first cases)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))

      (auto-constant-mutate-plush-one-case-more-steps (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))

(defn lexicase-with-most-important-case-selection-any-genetic-operator
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order.
  Also, adds to the selected individual which case was most important in its
  selection."
  [pop generation argmap]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         last-case (first cases)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (auto-apply-genetic-operator-plush-one-case (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))

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
      
      (let [mutated-ind (auto-mutate-plush-one-case 
        (assoc (lrand-nth survivors) :most-important-case last-case) generation argmap) ]
        (if (< (lrand) 0.5)
          (auto-constant-mutate-plush-one-case mutated-ind generation argmap))
        mutated-ind)

      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases)
               (first cases))))))

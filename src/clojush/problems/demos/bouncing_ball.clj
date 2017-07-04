(ns clojush.problems.demos.bouncing-ball
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojure.math.numeric-tower]
        [clojush pushstate interpreter]
        clojush.instructions.common))

;; program to solve the bouncing ball problem
;; each case in form  [in1 in2 in3 correct_output]
;; Inputs are initial height, height after first bounce and number of bounces

(defn bounce
  [height distance number_of_bounces]
  (let [bounciness_index (/ distance height)]
    (loop [iteration 0 
           height height
           distance distance
           total_distance 0]
      (let [new_distance (* distance bounciness_index)]
        (if (>= iteration number_of_bounces)
          total_distance
          (recur (inc iteration) distance new_distance (+ total_distance distance height))))     
      )
    )
  )

(defn error-function
  [program]
  (doall
   (for [[input1 input2 input3] (repeatedly 100 #(vector (+ 100.0 (rand 101))
                                                         (inc (rand 150))
                                                         (inc (rand-int 10))))]
        (let [state (run-push program
                              (push-item input3 :input
                                         (push-item input2 :input
                                                    (push-item input1 :input
                                                               (make-push-state)))))
              top-float (top-item :float state)]
             (if (number? top-float)
                 (abs (- top-float 
                         (bounce input1 input2 input3)))
                 1000000)))))

(def argmap
  {
   :error-function error-function
   :atom-generators (concat (registered-for-stacks [:integer :boolean :float :exec])
                            (list (fn [] (lrand-int 100))
                                  'in1
                                  'in2
                                  'in3))
   :max-points 500
   :max-genome-size-in-initial-program 125
   :evalpush-limit 1000
   :population-size 1000
   :max-generations 300
   :parent-selection :epsilon-lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :error-threshold 1.0
   })

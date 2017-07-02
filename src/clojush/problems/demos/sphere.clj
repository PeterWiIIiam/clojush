(ns clojush.problems.demos.sphere
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;;;;;;;;;;;;
;; Volume of a sphere 

(defn sphere-volume
  [radius]
  (* (/ 4 3) (* 3.142 (* radius radius radius))))

(def argmap
  {:error-function (fn [program]
                     (doall
                      (for [input (range 0 20 0.5)]
                         (let [state (run-push program 
                                               (push-item input :input 
                                                          (make-push-state)))
                               top-float (top-item :float state)]
                           (if (number? top-float)
                             (abs (- top-float 
                                     (sphere-volume input)))
                             1000000)))))    
   :atom-generators (list (fn [] (lrand 10))
                          'in1
                          'float_div
                          'float_mult
                          'float_add
                          'float_sub
                          3.142)
   :max-points 500
   :max-genome-size-in-initial-program 125
   :evalpush-limit 500
   :population-size 1000
   :max-generations 300
   :epigenetic-markers []
   :parent-selection :epsilon-lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.3
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :error-threshold 0.1
   :uniform-mutation-constant-tweak-rate 0
   })


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
   :max-points 200
   :max-genome-size-in-initial-program 200
   :evalpush-limit 200
   :population-size 1000
   :max-generations 300
   :epigenetic-markers []
   :parent-selection :epsilon-lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.3
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :error-threshold 0.1
   })


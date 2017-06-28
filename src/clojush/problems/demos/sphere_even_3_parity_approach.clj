(ns clojush.problems.demos.sphere-even-3-parity-approach
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

;; sphere_even_3_parity_approach.clj

;; each case in form  [in1 correct_output]


(def target-data
  [[1 (/ 1571 375)]
   [2 (/ 12568 375)]
   [3 (/ 14139 125)]
   [4 (/ 100544 375)]
   [5 (/ 1571 3)]
   [6 904.896]
   [7 1436.941333]
   [8 2144.938667]])



(def argmap
  {:error-function (fn [program]
                     (doall
                       (for [[input1 output] target-data]
                         (let [state (run-push program 
                                               (push-item input1 :input 
                                                          (make-push-state)))
                               top-int (top-item :integer state)]
                           (if (not (= top-int :no-stack-item))
                             (if (= top-int output) 0 1)
                             1000)))))
   :atom-generators (concat (registered-for-stacks [:integer :boolean :code :exec])
                            (list (fn [] (lrand-int 100))
                                  'in1
                                  ))
   })


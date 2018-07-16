(ns clojush.simplification
  (:use [clojush util globals pushstate random individual evaluate translate]
        clojush.pushgp.genetic-operators
        clojush.instructions.tag))

(def post-selection-genetic-operators
  {:uniform-addition-and-deletion {:fn uniform-addition-and-deletion :parents 1}
   :uniform-constant-mutation {:fn uniform-constant-mutation :parents 1}
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-simplification

(defn remove-paren-pair
  "Removes one random pair of parens from a program. Cannot remove outermost pair."
  [program]
  (if (not (seq? program))
    program
    (let [open-close-seq (list-to-open-close-sequence program)
          number-parens (count (filter #(= :open %) open-close-seq))]
      (if (<= number-parens 1)
        program
        (let [pair-to-remove (inc (lrand-int (dec number-parens))) ;don't want first pair
              pair-removed-program (loop [open-count 0
                                          close-diff max-number-magnitude
                                          program-so-far []
                                          rest-program open-close-seq]
                                     (cond
                                       (nil? rest-program) 
                                       (apply list program-so-far)
                                       ;
                                       (= (first rest-program) :open) 
                                       (if (== open-count pair-to-remove)
                                         (recur (inc open-count)
                                                1
                                                program-so-far
                                                (next rest-program))
                                         (recur (inc open-count)
                                                (inc close-diff)
                                                (conj program-so-far (first rest-program))
                                                (next rest-program)))
                                       ;
                                       (= (first rest-program) :close) 
                                       (if (zero? (dec close-diff))
                                         (recur open-count
                                                max-number-magnitude
                                                program-so-far
                                                (next rest-program))
                                         (recur open-count
                                                (dec close-diff)
                                                (conj program-so-far (first rest-program))
                                                (next rest-program)))
                                       ;
                                       :else 
                                       (recur open-count
                                              close-diff
                                              (conj program-so-far (first rest-program))
                                              (next rest-program))))]
          (open-close-sequence-to-list pair-removed-program))))))

(defn auto-simplify 
  "Auto-simplifies the provided individual."
  ([ind error-function steps print? progress-interval]
    (auto-simplify ind error-function steps print? progress-interval false))
  ([ind error-function steps print? progress-interval maintain-ancestors]
    (when print? (printf "\nAuto-simplifying with starting size: %s" (count-points (:program ind))))
    (loop [step 0 program (:program ind) errors (:errors ind) total-errors (:total-error ind)]
      (when (and print? 
                 (or (>= step steps)
                     (zero? (mod step progress-interval))))
        (printf "\nstep: %s\nprogram: %s\nerrors: %s\ntotal: %s\nsize: %s\n" 
                step (pr-str (not-lazy program)) (not-lazy errors) total-errors (count-points program))
        (flush))
      (if (>= step steps)
        (make-individual :program program :errors errors :total-error total-errors 
                         :history (:history ind) 
                         :ancestors (if maintain-ancestors
                                      (cons (:program ind) (:ancestors ind))
                                      (:ancestors ind))
                         :genetic-operators :simplification)
        (let [new-program (if (< (lrand-int 5) 4)
                            ;; remove a small number of random things
                            (loop [p program how-many (inc (lrand-int 2))]
                              (if (or (zero? how-many)
                                      (<= (count-points p) 1))
                                p
                                (recur (remove-code-at-point p (inc (lrand-int (dec (count-points p)))))
                                       (dec how-many))))
                            ;; remove single paren pair
                            (remove-paren-pair program))
              new-errors (:errors (error-function {:program new-program}))
              new-total-errors (compute-total-error new-errors)] ;simplification bases its decision on raw error; HAH-error could also be used here
          (if (= new-errors errors) ; only keep the simplified program if its error vector is the same as the original program's error vector
            (recur (inc step) new-program new-errors new-total-errors)
            (recur (inc step) program errors total-errors)))))))

(defn auto-simplify-from-program
  [p error-function steps print? progress-interval]
  (let [errs (:errors (error-function {:program p}))]
    (auto-simplify (make-individual :program p
                                    :errors errs
                                    :total-error (reduce + errs)
                                    :genetic-operators :simplification)
                   error-function
                   steps
                   print?
                   progress-interval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-simplification of Plush genomes

(defn select-random-weighted-item
  "Given map of item/probability key/values, picks an item probabilistically based on the probabilities."
  [prob-map]
  (let [summed-probs (reductions (fn [[_ prev-prob] [item prob]]
                                   (vector item (+ prev-prob prob)))
                                 prob-map)
        rand-num (rand (second (last summed-probs)))]
    (loop [items-and-probs summed-probs]
      (if (< rand-num (second (first items-and-probs)))
        (first (first items-and-probs))
        (recur (rest items-and-probs))))))

(defn choose-random-k-without-replacement
  "Selects k random elements of input list, without replacement. If fewer than k exist, selects all."
  [k elements]
  (take k (shuffle elements)))

(defn change-silent-at-indices
  "Changes the value of the :silent tag on the gene at each index of indices into new-value."
  [genome indices new-value]
  (if (empty? indices)
    genome
    (recur (assoc-in genome [(first indices) :silent] new-value)
           (rest indices)
           new-value)))

(defn apply-simplification-step-to-genome
  "Takes a genome and a map of transformation/probability pairs. Picks a transformation
   probabilistically, and then applies it to the genome by silencing/unsilencing/no-oping
   random genes that aren't already of that type."
  [genome simplification-step-probabilities]
  (let [transformation-map (select-random-weighted-item simplification-step-probabilities)
        silencings (get transformation-map :silence 0)
        unsilencings (get transformation-map :unsilence 0)
        no-opings (get transformation-map :no-op 0)
        silent-values (map :silent genome)
        indices-available-to-silence (if (> silencings 0)
                                       (map second
                                            (remove #(= (first %) true)
                                                    (map vector silent-values (range))))
                                       '())
        indices-available-to-unsilence (if (> unsilencings 0)
                                         (map second
                                              (remove #(or (= (first %) false)
                                                           (= (first %) nil))
                                                      (map vector silent-values (range))))
                                         '())
        indices-available-to-no-op (if (> no-opings 0)
                                     (map second
                                          (remove #(= (first %) :no-op)
                                                  (map vector silent-values (range))))
                                     '())
        indices-to-silence (choose-random-k-without-replacement silencings indices-available-to-silence)
        indices-to-unsilence (choose-random-k-without-replacement unsilencings indices-available-to-unsilence)
        indices-to-no-op (choose-random-k-without-replacement no-opings indices-available-to-no-op)]
    ; Order of changes is: unsilence -> no-op -> silence
    ; This makes it so silencings take highest priority.
    (-> genome
      vec      ; Needs to be a vector for change-silent-at-indices
      (change-silent-at-indices indices-to-unsilence false)
      (change-silent-at-indices indices-to-no-op :no-op)
      (change-silent-at-indices indices-to-silence true))))

(defn auto-simplify-plush
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op"
  ([ind error-function steps print-progress-interval]
    (auto-simplify-plush ind error-function steps print-progress-interval
                         {{:silence 1} 0.5
                          {:silence 2} 0.3
                          {:silence 3} 0.1
                          {:silence 4} 0.1
                          ;{:silence 1 :unsilence 1} 0.05  ;Not used by default
                          ;{:silence 2 :unsilence 1} 0.1   ;Not used by default
                          ;{:silence 3 :unsilence 1} 0.05  ;Not used by default
                          ;{:no-op 1} 0.05                 ;Not used by default
                          }))
  ([ind error-function steps print-progress-interval simplification-step-probabilities]
    (when (not (zero? print-progress-interval))
      (printf "\nAuto-simplifying Plush genome with starting size: %s" (count (:genome ind))))
    (loop [step 0
           genome (:genome ind)
           program (if (:program ind)
                     (:program ind)
                     (translate-plush-genome-to-push-program ind
                                                             {:max-points (* 10 (count genome))}))
           errors (if (:errors ind)
                    (:errors ind)
                    (:errors (error-function ind)))]
      (when (and (not (zero? print-progress-interval))
                 (or (>= step steps)
                     (zero? (mod step print-progress-interval))))
        (println "\nstep:" step)
        (println "genome:" (pr-str (not-lazy genome)))
        (println "program:" (pr-str (not-lazy program)))
        (println "errors:" (not-lazy errors))
        (println "genome size:" (count genome))
        (println "program size:" (count-points program)))
      (if (>= step steps)
        (make-individual :genome genome :program program :errors errors :total-error (apply + errors)
                         :history (:history ind) :genetic-operators :simplification)
        (let [new-genome (apply-simplification-step-to-genome genome simplification-step-probabilities)
              new-program (translate-plush-genome-to-push-program (assoc ind :genome new-genome)
                                                                  {:max-points (* 10 (count genome))})
              new-errors (:errors (error-function {:program new-program}))]
          (if (and (= new-errors errors)
                   (<= (count-points new-program) (count-points program)))
            (recur (inc step) new-genome new-program new-errors)
            (recur (inc step) genome program errors)))))))

(defn auto-simplify-plush-one-case
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op"

  ([ind generation argmap]
   (auto-simplify-plush-one-case ind (:error-function argmap) 1  1 ))
  ([ind error-function steps print-progress-interval]
   (println "current steps are" steps)
    (auto-simplify-plush-one-case ind error-function steps print-progress-interval
                         {{:silence 1} 0.5
                          {:silence 2} 0.3
                          {:silence 3} 0.1
                          {:silence 4} 0.1
                          ;{:silence 1 :unsilence 1} 0.05  ;Not used by default
                          ;{:silence 2 :unsilence 1} 0.1   ;Not used by default
                          ;{:silence 3 :unsilence 1} 0.05  ;Not used by default
                          ;{:no-op 1} 0.05                 ;Not used by default
                          }))
  ([ind error-function steps print-progress-interval simplification-step-probabilities]
    (when (not (zero? print-progress-interval))
      (printf "\nAuto-simplifying Plush genome with starting size: %s" (count (:genome ind))))
    (let [case-per-input 2
          most-important-case (:most-important-case ind)]
     (loop [step 0
            genome (:genome ind)
            program (if (:program ind)
                      (:program ind)
                      (translate-plush-genome-to-push-program ind
                                                              {:max-points (* 10 (count genome))}))
            errors (nth (error-function ind 
                                        (quot most-important-case case-per-input)) (rem most-important-case case-per-input))]
       (when (and (not (zero? print-progress-interval))
                  (or (>= step steps)
                      (zero? (mod step print-progress-interval))))
         (println "\nstep:" step)
         ;; (println "genome:" (pr-str (not-lazy genome)))
         ;; (println "program:" (pr-str (not-lazy program)))
         ;; (println "errors:" (not-lazy errors))
         (println "genome size:" (count genome))
         (println "program size:" (count-points program)))
         
       (if (>= step steps)
         (assoc ind :genome genome :program program)
         (let [new-genome (apply-simplification-step-to-genome genome simplification-step-probabilities)
               new-program (translate-plush-genome-to-push-program (assoc ind :genome new-genome)
                                                                   {:max-points (* 10 (count genome))})
               new-errors (nth (error-function {:program new-program} (quot most-important-case case-per-input)) (rem most-important-case case-per-input))]
                                        ;(println "most important case for the inidividual is" (:most-important-case ind))
           (println "new error" new-errors "error" errors)
           (println "most important case" most-important-case "input number" (quot most-important-case case-per-input) 
                    "case number" (rem most-important-case case-per-input))

           (if (and (<= new-errors errors)
                    (<= (count-points new-program) (count-points program)))
             (recur (inc step) new-genome new-program new-errors)
             (recur (inc step) genome program errors))))))))





(defn auto-mutate-plush-one-case
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op"

  ([ind generation argmap]
   (auto-mutate-plush-one-case ind argmap 20  0))
  ([ind argmap steps print-progress-interval]
   (println "current steps are" steps)
    (auto-mutate-plush-one-case ind argmap steps print-progress-interval
                         {{:silence 1} 0.5
                          {:silence 2} 0.3
                          {:silence 3} 0.1
                          {:silence 4} 0.1
                          ;{:silence 1 :unsilence 1} 0.05  ;Not used by default
                          ;{:silence 2 :unsilence 1} 0.1   ;Not used by default
                          ;{:silence 3 :unsilence 1} 0.05  ;Not used by default
                          ;{:no-op 1} 0.05                 ;Not used by default
                          }))
  ([ind {:keys [error-function uniform-addition-and-deletion-rate atom-generators] :as argmap} 
    steps print-progress-interval simplification-step-probabilities]
    (when (not (zero? print-progress-interval))
      (printf "\nAuto-simplifying Plush genome with starting size: %s" (count (:genome ind))))
    (let [case-per-input 2
          most-important-case (:most-important-case ind)]
   
     (loop [step 0
            genome (:genome ind)
            errors (nth (error-function ind (quot most-important-case case-per-input)) (rem most-important-case case-per-input))]
       (when (and (not (zero? print-progress-interval))
                  (or (>= step steps)
                      (zero? (mod step print-progress-interval))))
         (println "\nstep:" step)
         ;; (println "genome:" (pr-str (not-lazy genome)))
         ;; (println "program:" (pr-str (not-lazy program)))
         ;; (println "errors:" (not-lazy errors))
         (println "genome size:" (count genome))
         )
         
       (if (>= step steps)
         (assoc ind :genome genome )

         (let [addition-rate (random-element-or-identity-if-not-a-collection uniform-addition-and-deletion-rate)
               deletion-rate (if (zero? addition-rate)
                               0
                               (/ 1 (+ (/ 1 addition-rate) 1)))
               after-addition (vec (apply concat
                                          (mapv #(if (< (lrand) addition-rate)
                                                   (lshuffle [% 
                                                              (random-plush-instruction-map
                                                               atom-generators argmap)])
                                                   [%])
                                                (:genome ind))))
               new-genome (vec (filter identity
                                       (mapv #(if (< (lrand) deletion-rate) nil %)
                                             after-addition)))               
               new-program  (translate-plush-genome-to-push-program (assoc ind :genome new-genome)
                                                                    {:max-points (* 10 (count genome))})
               printing (println "case number"  (int (/ (:most-important-case ind) case-per-input)))
               new-errors (nth (error-function {:program new-program} (quot most-important-case case-per-input)) 
                               (rem most-important-case case-per-input))]                                        
           
           (println "new -erorrs" new-errors)
           (println "errors" errors)
           (println "most important case" most-important-case)
           (println "input number" (quot most-important-case case-per-input))
           (println "case number" (rem most-important-case case-per-input))

           (if (<= new-errors errors)
             (recur (inc step) new-genome  new-errors)
             (recur (inc step) genome  errors))))))))





(defn auto-mutate-plush-one-case-more-steps
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op"

  ([ind generation argmap]
   (println "first arity")
   (auto-mutate-plush-one-case-more-steps ind argmap 100  0 ))
  ([ind argmap steps print-progress-interval] 
   (println "second arity")
    (auto-mutate-plush-one-case-more-steps ind argmap steps print-progress-interval
                         {{:silence 1} 0.5
                          {:silence 2} 0.3
                          {:silence 3} 0.1
                          {:silence 4} 0.1
                          ;{:silence 1 :unsilence 1} 0.05  ;Not used by default
                          ;{:silence 2 :unsilence 1} 0.1   ;Not used by default
                          ;{:silence 3 :unsilence 1} 0.05  ;Not used by default
                          ;{:no-op 1} 0.05                 ;Not used by default
                          }))
  ([ind {:keys [error-function uniform-addition-and-deletion-rate atom-generators] :as argmap}
    steps print-progress-interval simplification-step-probabilities] 
   (println "call lexicase more steps haha")
   (when (not (zero? print-progress-interval))
     (printf "\nAuto-simplifying Plush genome with starting size: %s" (count (:genome ind)))) 
   (let [case-per-input 2
         most-important-case (:most-important-case ind)
         evaluation-interval 5]
     (println "case - per - input" case-per-input)
     (loop [step 0
            genome (:genome ind)
            errors (nth (error-function ind (quot most-important-case case-per-input)) (rem most-important-case case-per-input))]
       (when (and (not (zero? print-progress-interval))
                  (or (>= step steps)
                      (zero? (mod step print-progress-interval))))
         (println "\nstep:" step)
         ;; (println "genome:" (pr-str (not-lazy genome)))
         ;; (println "program:" (pr-str (not-lazy program)))
         ;; (println "errors:" (not-lazy errors))
         (println "genome size:" (count genome)))
       (if (>= step steps)     
          (assoc ind :genome genome)
          (let [addition-rate (random-element-or-identity-if-not-a-collection uniform-addition-and-deletion-rate)
                deletion-rate (if (zero? addition-rate)
                                0
                                (/ 1 (+ (/ 1 addition-rate) 1)))
                after-addition (vec (apply concat
                                           (mapv #(if (< (lrand) addition-rate)
                                                    (lshuffle [% 
                                                               (random-plush-instruction-map
                                                                atom-generators argmap)])
                                                    [%])
                                                 (:genome ind))))
                new-genome (vec (filter identity
                                        (mapv #(if (< (lrand) deletion-rate) nil %)
                                              after-addition)))    
                new-program  (translate-plush-genome-to-push-program {:genome new-genome}
                                                                     {:max-points (* 10 (count genome))})
                compute-new-errors (= 0 (rem step evaluation-interval))
                new-errors
                (if compute-new-errors
                  (nth (error-function {:program new-program} (quot most-important-case case-per-input))
                       (rem most-important-case case-per-input))
                  errors)]

            (println "computer new errors" compute-new-errors "step" step)
            (println "new-erorrs" new-errors)
            (println "errors" errors compute-new-errors)
            (println "most important casessssssss" most-important-case)
            (println "input number" (quot most-important-case case-per-input))
            (println "case number" (rem most-important-case case-per-input))
            
            (if (<= new-errors errors)
              (recur (inc step) new-genome  new-errors)
              (recur (inc step) genome  errors))))))))

(defn auto-constant-mutate-plush-one-case
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op"

  ([ind generation argmap]
   (auto-mutate-plush-one-case ind argmap 10  1 ))
  ([ind argmap steps print-progress-interval]
   (println "current steps are" steps)
    (auto-mutate-plush-one-case ind argmap steps print-progress-interval
                         {{:silence 1} 0.5
                          {:silence 2} 0.3
                          {:silence 3} 0.1
                          {:silence 4} 0.1
                          ;{:silence 1 :unsilence 1} 0.05  ;Not used by default
                          ;{:silence 2 :unsilence 1} 0.1   ;Not used by default
                          ;{:silence 3 :unsilence 1} 0.05  ;Not used by default
                          ;{:no-op 1} 0.05                 ;Not used by default
                          }))
  ([ind {:keys [error-function uniform-mutation-rate uniform-mutation-constant-tweak-rate
               uniform-mutation-float-gaussian-standard-deviation
               uniform-mutation-int-gaussian-standard-deviation
               uniform-mutation-tag-gaussian-standard-deviation
               uniform-mutation-string-char-change-rate
               atom-generators] :as argmap} 
    steps print-progress-interval simplification-step-probabilities]
    (when (not (zero? print-progress-interval))
      (printf "\nAuto-simplifying Plush genome with starting size: %s" (count (:genome ind))))
    (let [case-per-input 2
          most-important-case (:most-important-case ind)]
   
     (loop [step 0
            genome (:genome ind)
            errors (nth (error-function ind (quot most-important-case case-per-input)) (rem most-important-case case-per-input))]
       (when (and (not (zero? print-progress-interval))
                  (or (>= step steps)
                      (zero? (mod step print-progress-interval))))
         (println "\nstep:" step)
         ;; (println "genome:" (pr-str (not-lazy genome)))
         ;; (println "program:" (pr-str (not-lazy program)))
         ;; (println "errors:" (not-lazy errors))
         (println "genome size:" (count genome))
         )
         
       (if (>= step steps)
         (assoc ind :genome genome )

         (let [uniform-mutation-rate 
               (random-element-or-identity-if-not-a-collection uniform-mutation-rate)
                
               uniform-mutation-constant-tweak-rate 
               (random-element-or-identity-if-not-a-collection uniform-mutation-constant-tweak-rate)
                
               uniform-mutation-float-gaussian-standard-deviation 
               (random-element-or-identity-if-not-a-collection uniform-mutation-float-gaussian-standard-deviation)
                
               uniform-mutation-int-gaussian-standard-deviation 
               (random-element-or-identity-if-not-a-collection uniform-mutation-int-gaussian-standard-deviation)
                
               uniform-mutation-tag-gaussian-standard-deviation 
               (random-element-or-identity-if-not-a-collection uniform-mutation-tag-gaussian-standard-deviation)
                
               uniform-mutation-string-char-change-rate 
               (random-element-or-identity-if-not-a-collection uniform-mutation-string-char-change-rate)
                
               string-tweak (fn [st]
                              (apply str (map (fn [c]
                                                (if (< (lrand) uniform-mutation-string-char-change-rate)
                                                  (lrand-nth (vec (concat ["\n" "\t"] 
                                                                          (map (comp str char) 
                                                                               (range 32 127)))))
                                                  c))
                                              st)))
               constant-mutator (fn [token]
                                  (let [const (:instruction token)]
                                    (if (tag-instruction? const)
                                      (tag-gaussian-tweak token 
                                                          uniform-mutation-tag-gaussian-standard-deviation)
                                      (assoc token
                                             :instruction
                                             (cond
                                               ;; float
                                               (float? const) 
                                               (perturb-with-gaussian-noise 
                                                uniform-mutation-float-gaussian-standard-deviation const)
                                               ;; integer
                                               (integer? const) 
                                               (Math/round (perturb-with-gaussian-noise 
                                                       uniform-mutation-int-gaussian-standard-deviation const))
                                               ;; string
                                               (string? const) 
                                               (string-tweak const)
                                               ;; boolean
                                               (or (= const true) (= const false)) 
                                               ;; anything else
                                               (lrand-nth [true false])
                                               :else 
                                               (:instruction 
                                                (first (random-plush-genome 1 atom-generators argmap))))))))
               token-mutator (fn [token]
                               (if (< (lrand) uniform-mutation-rate)
                                 (if (< (lrand) uniform-mutation-constant-tweak-rate)
                                   (constant-mutator ))
                                 token))
               new-genome (mapv token-mutator (:genome ind))
              
               new-program  (translate-plush-genome-to-push-program (assoc ind :genome new-genome)
                                                                    {:max-points (* 10 (count genome))})
               printing (println "case number"  (int (/ (:most-important-case ind) case-per-input)))
               new-errors (nth (error-function {:program new-program} (quot most-important-case case-per-input)) 
                               (rem most-important-case case-per-input))]                                        
           
           (println "new-erorrs" new-errors)
           (println "errors" errors)
           (println "most important case" most-important-case)
           (println "input number" (quot most-important-case case-per-input))
           (println "case number" (rem most-important-case case-per-input))

           (if (<= new-errors errors)
             (recur (inc step) new-genome  new-errors)
             (recur (inc step) genome  errors))))))))



(defn auto-apply-genetic-operator-plush-one-case
  "Automatically simplifies the genome of an individual without changing its error vector on
   the training set, based on the error-function. steps is the number of hill-climbing evaluations
   to test. print-progress-interval is how often to print progress of the simplification; if it is
   set to 0, then nothing will be printed.
   simplification-step-probabilities is a map of probabilities that are used to select what change
   to make during each step of the simplification. Each change is represented as a map with the
   following options for the keys, each of which has an integer of how many of those changes to make:
     :silence - number of unsilenced or no-op genes to set :silent = true
     :unsilence - number of silenced or no-op genes to set :silent = false
     :no-op - number of unsilenced or silenced genes to set :silent = :no-op"

  ([ind generation argmap]
   (auto-mutate-plush-one-case ind argmap 100 0))
  ([ind {:keys [error-function uniform-addition-and-deletion-rate atom-generators] :as argmap} 
    steps print-progress-interval]
    (when (not (zero? print-progress-interval))
      (printf "\nAuto-simplifying Plush genome with starting size: %s" (count (:genome ind))))
    (let [case-per-input 2
          most-important-case (:most-important-case ind)]
   
     (loop [step 0
            genome (:genome ind)
            errors (nth (error-function ind (quot most-important-case case-per-input)) (rem most-important-case case-per-input))]
       (when (and (not (zero? print-progress-interval))
                  (or (>= step steps)
                      (zero? (mod step print-progress-interval))))
         (println "\nstep:" step)
         ;; (println "genome:" (pr-str (not-lazy genome)))
         ;; (println "program:" (pr-str (not-lazy program)))
         ;; (println "errors:" (not-lazy errors))
         (println "genome size:" (count (:genoem ind))
         )
         
       (if (>= step steps)
         (assoc ind :genome genome)
         (let [new-ind   (((:post-selection-genetic-operator argmap) post-selection-genetic-operators) ind argmap)
               new-genome (:genome new-ind)
               new-program  (translate-plush-genome-to-push-program {:genome new-genome}
                                                                    {:max-points (* 10 (count genome))})
             
               printing (println "case number"  (int (/ (:most-important-case ind) case-per-input)))
               new-errors (nth (error-function {:program new-program} (quot most-important-case case-per-input)) 
                               (rem most-important-case case-per-input))]                                        
           
           (println "new -erorrs" new-errors)
           (println "errors" errors)
           (println "most important case" most-important-case)
           (println "input number" (quot most-important-case case-per-input))
           (println "case number" (rem most-important-case case-per-input))

           (if (<= new-errors errors)
             (recur (inc step) new-genome  new-errors)
             (recur (inc step) genome  errors)))))))))


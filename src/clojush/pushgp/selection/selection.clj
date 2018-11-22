(ns clojush.pushgp.selection.selection
  (:use [clojush globals random]
        [clojush.pushgp.selection preselection tournament lexicase epsilon-lexicase
         elitegroup-lexicase random-threshold-lexicase random-toggle-lexicase 
         randomly-truncated-lexicase lexicase-with-most-important-case novelty]))

(defn select
  "Returns a selected parent."
  [pop generation {:keys [parent-selection print-selection-counts] :as argmap}]
  (let [pop-with-meta-errors (map #(update-in % [:errors] (comp vec concat) (:meta-errors %)) pop)
        preselected (preselect pop-with-meta-errors argmap)
        selected (case parent-selection
                   :tournament (tournament-selection preselected argmap)
                   :lexicase (lexicase-selection preselected argmap)
                   :epsilon-lexicase (epsilon-lexicase-selection preselected argmap)
                   :elitegroup-lexicase (elitegroup-lexicase-selection preselected argmap)
                   :random-threshold-lexicase (random-threshold-lexicase-selection 
                                                preselected argmap)
                   :random-toggle-lexicase (random-toggle-lexicase-selection 
                                             preselected argmap)
                   :randomly-truncated-lexicase (randomly-truncated-lexicase-selection
                                                  preselected argmap)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (lrand-nth preselected)
                                     (lexicase-selection preselected argmap))
                   :lexicase-with-most-important-case (lexicase-with-most-important-case-selection preselected generation argmap)
                   :lexicase-with-most-important-case-mutate (lexicase-with-most-important-case-selection-mutate preselected generation argmap)
                   :lexicase-with-most-important-case-mutate-more-steps (lexicase-with-most-important-case-selection-mutate-more-steps preselected generation argmap)
                   :lexicase-with-most-important-case-any-genetic-operator (lexicase-with-most-important-case-selection-any-genetic-operator preselected generation argmap)
                   :lexicase-with-most-important-case-constant-mutate-more-steps (lexicase-with-most-important-case-selection-constant-mutate-more-steps preselected generation argmap)
                   :novelty-search (novelty-tournament-selection preselected argmap)
                   :uniform (lrand-nth preselected)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts 
             update-in 
             [(:uuid selected)] 
             (fn [sel-count] (if (nil? sel-count) 1 (inc sel-count)))))
    selected))


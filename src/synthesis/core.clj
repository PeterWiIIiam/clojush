;; synthesis.core.clj
;; The main code for query creation.
;; Tom Hemuth, thelmuth@cs.umass.edu, 2011

(ns synthesis.core
  (:require [clojure.contrib.sql :as sql]
            [clojush]
            [synthesis.db :as db]))

;;;;;;;;;;;;
;; A few things must be done in the clojush namespace.
(in-ns 'clojush)

;; Redefine push-types to include :select, :from, and :where, and then redefine the push state structure.
(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :zip :string :select :from :where))
(define-push-state-structure)

;;;;;;;;;;;;
;; Return to core namespace
(in-ns 'synthesis.core)

;;;;;;;;;;
;; Globals for creating constraints

(def comparators ["=" "<" ">" "<=" ">=" "<>"])
(def cols-map db/synthesis-db-columns-map)

;;;;;;;;;;
;; Normal stack instructions for SFW
(define-registered where_dup (duper :where))
(define-registered where_swap (swapper :where))
(define-registered where_rot (rotter :where))
; Other possibilities: pop, flush, eq, stackdepth, yank, yankdup, shove

;;;;;;;;;;
;; Helper functions for :where stack manipulation

(defn select-column
  "Returns column name based on index. Index is constrained within bounds of vector length by modulus."
  [index]
  (first (nth db/synthesis-db-columns (mod index (count db/synthesis-db-columns)))))

(defn get-column-type
  "Retrieves the column type for column."
  [column]
  (let [type (get cols-map column)]
    (cond
      (= type :int) :integer
      (string? type) (if (= "varchar" (subs type 0 7))
                       :string
                       nil)
      true nil)))

(defn get-constant-from-column
  "Gets a constant from column from row index. Optional argument distinct is a boolean that defines whether
   or not the values in the column should be distinct. Index is taken mod the number of options to always
   give a legal index."
  [column index distinct]
  (let [query (str "SELECT "
                   (if distinct
                     "DISTINCT "
                     "")
                   (name column)
                   " FROM adult")
        results (db/run-db-function db/synthesis-db db/db-query query)
        return (get (nth results
                         (mod index (count results)))
                    column)]
    (if (string? return)
      (str "'" return "'")
      return)))

;;;;;;;;;;
;; Instructions for :where stack manipulation

(clojush/define-registered
  where_constraint_from_stack
  (fn [state]
    (if (not (>= (count (get state :integer)) 2)) ; We will need at least 2 integers
      state
      (let [column (select-column (clojush/stack-ref :integer 0 state))
            column-type (get-column-type column)]
        (if (nil? column-type) ; Check for legit column-type
          state
          (if (or (empty? (get state column-type)) ; Make sure column type isn't empty
                  (and (= column-type :integer)
                       (not (>= (count (get state :integer)) 3))))
            state
            (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                   (count comparators)))
                  constant (cond
                             (= column-type :integer) (clojush/stack-ref :integer 2 state)
                             (= column-type :string) (str "'"
                                                          (clojush/stack-ref :string 0 state)
                                                          "'")
                             true (clojush/stack-ref column-type 0 state))
                  constraint (str (name column) " " comparator " " constant)]
              (clojush/push-item constraint :where
                                 (clojush/pop-item :integer
                                                   (clojush/pop-item :integer
                                                                     (clojush/pop-item column-type
                                                                                       state)))))))))))

; Uses a constant taken from the selected column, where options are not distinct.
; This makes probability of choosing constant C proportional to the frequency of C in the column.
(clojush/define-registered
  where_constraint_from_index
  (fn [state]
    (if (not (>= (count (get state :integer)) 3)) ; We will need 3 integers
      state
      (let [column (select-column (clojush/stack-ref :integer 0 state))
            column-type (get-column-type column)]
        (if (nil? column-type) ; Check for legit column-type
          state
          (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                 (count comparators)))
                constant (get-constant-from-column column
                                                   (clojush/stack-ref :integer 2 state)
                                                   false)
                constraint (str (name column) " " comparator " " constant)]
            (clojush/push-item constraint :where
                               (clojush/pop-item :integer
                                                 (clojush/pop-item :integer
                                                                   (clojush/pop-item :integer
                                                                                     state))))))))))

; Uses a constant taken from the selected column, where options are distinct.
; This makes each distinct option equally likely.
(clojush/define-registered
  where_constraint_distinct_from_index
  (fn [state]
    (if (not (>= (count (get state :integer)) 3)) ; We will need 3 integers
      state
      (let [column (select-column (clojush/stack-ref :integer 0 state))
            column-type (get-column-type column)]
        (if (nil? column-type) ; Check for legit column-type
          state
          (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                 (count comparators)))
                constant (get-constant-from-column column
                                                   (clojush/stack-ref :integer 2 state)
                                                   true)
                constraint (str (name column) " " comparator " " constant)]
            (clojush/push-item constraint :where
                               (clojush/pop-item :integer
                                                 (clojush/pop-item :integer
                                                                   (clojush/pop-item :integer
                                                                                     state))))))))))

(defn where-conjoiner
  [conjunction]
  (fn [state]
    (if (not (>= (count (get state :where)) 2))
      state
      (let [first-item (clojush/stack-ref :where 0 state)
            second-item (clojush/stack-ref :where 1 state)]
        (clojush/push-item (str "(" second-item " " conjunction " " first-item ")")
                           :where
                           (clojush/pop-item :where
                                             (clojush/pop-item :where state)))))))
            
(clojush/define-registered where_and (where-conjoiner "AND"))
(clojush/define-registered where_or (where-conjoiner "OR"))

(clojush/define-registered
  where_not
  (fn [state]
    (if (empty? (get state :where))
      state
      (let [item (clojush/stack-ref :where 0 state)]
        (clojush/push-item (str "(NOT " item ")")
                           :where
                           (clojush/pop-item :where state))))))

;;;;;;;;;;
;; Query from Examples
;;
;; SFW maps are of the following form:
;; :select ["column1" "column2" "column3" ... "columnN"]
;; :from ["table1" "table2" ... "tableM"]
;; :where ["clause1" 'AND/'OR "clause2" 'AND/'OR ... "clauseZ"]

(defn sfw-map-to-query-string
  "Takes a map of a SFW query and returns a string representation of that query."
  [swf-map]
  (str "SELECT " (apply str (interpose ", " (get swf-map :select))) \newline
       "FROM " (apply str (interpose ", " (get swf-map :from))) \newline
       "WHERE " (apply str (interpose " " (get swf-map :where)))))

#_(clojush/pushgp :error-function (fn [program]
                                    (list
                                      (let [embryo-query {:select []
                                                          :from []
                                                          :where []}
                                            final-state (clojush/run-push
                                                          program
                                                          (clojush/push-item embryo-query
                                                                             :auxiliary
                                                                             (clojush/make-push-state)))
                                            result-query (clojush/top-item :auxiliary final-state)]
                                        ;Now, need to create a SFW string, and use it on the database to find the fitness.
                                        ;----for now, just return a random integer
                                        (rand-int 1000))))
                  :atom-generators (list 'string_length
                                         'string_take
                                         'string_concat
                                         'and_constraint)
                  :population-size 100
                  :max-generations 50
                  :tournament-size 7)



;; Test sfw-map-to-query-string
#_(def ex-query
    (sfw-map-to-query-string {:select ["age" "education" "hours_per_week"]
                              :from ["adult"]
                              :where ["age > 50" 'AND "workclass = \"State-gov\""]}))

#_(db/run-db-function db/synthesis-db db/db-query ex-query)

;; Test a query
(time (db/run-db-function db/synthesis-db
                          db/db-query
                          "SELECT DISTINCT workclass
                           FROM adult
                           WHERE (NOT workclass > 'Private')"))

;; To get an indexed thing from the distinct things
#_(nth (db/run-db-function db/synthesis-db db/db-query "SELECT DISTINCT education
                                                        FROM adult
                                                        WHERE 0=0") 8)

;; To get an indexed thing from non-distinct things
#_(nth (db/run-db-function db/synthesis-db db/db-query "SELECT education
                                                        FROM adult
                                                        WHERE 0=0") 24212)

;; Test and_constraint
(println (clojush/run-push '(24 "thomas" 2 1 where_constraint_from_stack)
                           (clojush/make-push-state)))

(println (clojush/run-push '(4 "thomas" 2 1 where_constraint_from_index where_not)
                           (clojush/make-push-state)))

(clojush/run-push '("thomas" 7 2 1 where_constraint_distinct_from_index
                             539 2 10 where_constraint_distinct_from_index
                             14 90 3 where_constraint_from_index
                             where_and
                             4 13 214 where_constraint_from_stack
                             where_or)
                  (clojush/make-push-state))

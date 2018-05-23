(ns clojush.meta-error-test)


(defn meta-size
  [ind pop argmap]
  (count (:genome ind)))

(defn meta-total-error
  [ind pop argmap]
  (:total-error ind))

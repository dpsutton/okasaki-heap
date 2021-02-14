(ns dpsutton.heap-test
  (:require [dpsutton.heap :as h]
            [clojure.test :refer :all]))

(defn- heap-sort [heap]
    (loop [result []
           heap heap]
      (if-let [x (h/find-min heap)]
        (recur (conj result x) (h/delete-min heap))
        result)))

(defn- integers
  ([] (integers 1000))
  ([limit] (shuffle (range limit))))

(deftest heap-test
  (is (= (sort (integers))
         (heap-sort
           (transduce (map identity)
                      (fn queueueue
                        ([] (h/make-heap compare))
                        ([result] result)
                        ([result x] (h/insert x result)))
                      (integers))))))

(ns dpsutton.heap-test
  (:require [dpsutton.heap :as h]
            [clojure.test :refer :all]))

(defn- integers
  ([] (integers 1000))
  ([limit] (shuffle (range limit))))

(deftest heap-test
  (is (= nil (h/find-min (h/make-heap compare))))
  (is (= 3 (-> (h/make-heap compare)
               (h/insert 3)
               (h/find-min))))
  (is (= 3 (-> (h/make-heap compare)
               (h/insert 3)
               (h/insert 7)
               (h/find-min))))
  (is (= (sort (integers))
         (transduce (map identity)
                    (fn queueueue
                      ([] (h/make-heap compare))
                      ([result] (h/heap-sort result))
                      ([result x] (h/insert result x)))
                    (integers))))

  (is (= 1 (count (h/insert (h/make-heap compare) 1))))
  (let [h (reduce conj (h/make-heap compare) (repeat 40 40))]
    (is (= 40 (count h)))
    (is (= 40 (h/find-min h)))))

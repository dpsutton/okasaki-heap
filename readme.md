## Okasaki Heap

This is a translation of Okasaki's Heap ([page 73](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)) to Clojure.

```clojure
(require '[dpsutton.heap :as h])

(->> (range 2000)
     (shuffle)
     (reduce h/insert (h/make-heap compare))
     (h/heap-sort)
     (take 4))
;; (0 1 2 3)
```

Shortest path through a graph. Two paths [:a :c :d] and [:a :b :c :d]. A bit of a derivative of djikstra's algo slash just breadth first search looking for the destination.
```clojure
(let [graph {:start #{:a}
             :a     #{:b :c}
             :b     #{:c :d}
             :c     #{:d}
             :d     #{}}
      cmp   (fn [x y] (compare (count (:path x)) (count (:count y))))
      dest  :d]
  (loop [h    (h/insert (h/make-heap cmp)
                        {:node :start :path [:start]})
         seen #{:start}]
    (let [{:keys [node path]} (h/find-min h)
          nbs  (->> (get graph node) (remove seen) set)]
      (if (contains? nbs dest)
        (conj path dest)
        (recur (reduce #(h/insert %1 {:node %2 :path (conj path %2)})
                       (h/delete-min h)
                       nbs)
               (into seen nbs))))))
[:start :a :c :d]
```

The heap is `clojure.lang.Seqable` so supports `seq`. This returns a seq of the items in the heap but they are not sorted as seq needs to be cheap. Heaps are also `clojure.lang.IPersistentCollection` so support `cons` (and therefore `conj`), `empty`, and `equiv`. Two heaps are equivalent if they have the same cardinality and same sort order. There's no good way to enforce that the comparison functions are the same, so can only look at the result of the comparison. Heaps are also `clojure.lang.Counted` so `count` is supported and O(1).

The heap interface that you should use is `insert` (equivalent to `conj`), `find-min`, `delete-min`, and `heap-sort` for the sorted results.

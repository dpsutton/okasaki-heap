(ns dpsutton.heap
  "Okasaki binary heap"
  (:require [clojure.spec.alpha :as s]))

;; datatype Tree = Node of int * Element x Tree list ;; integer is rank of the tree
;; type Heap = Tree list

;; val empty = []
;; fun isEmpty ts = null ts
;; fun rank (Node (r, x, c)) = r
;; fun root (Node (r, x, c)) = x
;; fun link (t1 as Node (r, x1, c1), t2 as Node (_, x2, c2)) =
;;      if Elem.leq (x1, x2) then Node (r+1, x1, t2::c1) else Node (r+1, x2, t1::c2)
;; fun insTree (t, []) = [t]
;;   | insTree (t1, ts as t2::rest) =
;;       if rank t1 < rank t2 then t1::ts else insTree (link (t1, t2), rest)

;; fun insert (x, ts) = insTree (Node (0, x, []), ts)
;; fun merge (ts1, []) = ts1
;;   | merge ([], ts2) = ts2
;;   | merge (t1::ts1, t2::ts2) =
;;       if rank t1 < rank t2 then t1::merge (ts1, t2::ts2)
;;       else if rank t2 < rank t1 then t2::merge (t1::ts1, ts2)
;;       else insTree (link (t1, t2)), merge (ts1, ts2)

;; fun findMin [] = raise EMPTY
;;   | findMin [t] = root t
;;   | findMin (t::ts) = let val x = root t
;;                           val y = findMin ts
;;                       in if Elem.leq (x,y) then x else y end

;; fun deleteMin [] = raise EMPTY
;;   | deleteMin ts =
;;       let fun getMin [t] = (t, [])
;;             | getMin (t::ts) =
;;                 let val (t', ts') = getMin ts
;;                 in if Elem.leq (root t, root t') then (t, ts) else (t', t::ts') end
;;       val (Node (_, x, ts1), ts2) = getMin ts
;;       in merge (rev ts1, ts2) end

(defprotocol INode
  (rank [_])
  (element [_])
  (children [_])
  (link [cmp n1 n2]))

(defn leq [cmp x y]
  (<= (cmp x y) 0))

(deftype Node [rnk e chdren]
  INode
  (rank [_] rnk)
  (element [_] e)
  (children [_] chdren)
  (link [n1 n2 cmp]
    (if (leq cmp e (element n2))
      (Node. (inc rnk) e            (cons n2 chdren))
      (Node. (inc rnk) (element n2) (cons n1 (children n2))))))

(declare heap?)

(defn node? [node]
  (and (instance? Node node)
       (every? heap? (children node))))

(defn heap? [x]
  (and (sequential? x) (every? node? x)))

(s/def ::heap heap?)
(s/def ::node node?)

(s/fdef insert-tree*
  :args (s/cat :cmp fn? :heap ::heap :node ::node))

(defn- insert-tree* [cmp heap node]
  (if (empty? heap)
    (list node)
    (let [[t2 & rst] heap]
      (if (< (rank node) (rank t2))
        (cons node heap)
        (recur cmp rst (link node t2 cmp))))))

(s/fdef merge-heap*
  :args (s/cat :cmp fn? :tree1 ::heap :tree2 ::heap)
  :ret ::heap)

(defn- merge-heap* [cmp [t1 & ts1 :as tree1] [t2 & ts2 :as tree2]]
  (cond (empty? tree2)          tree1
        (empty? tree1)          tree2
        (< (rank t1) (rank t2)) (cons t1 (merge-heap* cmp ts1 tree2))
        (> (rank t1) (rank t2)) (cons t2 (merge-heap* cmp tree1 ts2))
        :else                   (insert-tree* cmp
                                              (merge-heap* cmp ts1 ts2)
                                              (link t1 t2 cmp))))

(s/fdef find-min*
  :args (s/cat :cmp fn? :heap ::heap)
  :ret ::heap)

(defn find-min* [cmp [node & nodes :as heap]]
  (cond (empty? heap)  nil
        (empty? nodes) (element node)
        :else          (let [element (element node)
                             min' (find-min* cmp nodes)]
                         (if (leq cmp element min') element min'))))

(s/fdef delete-min*
  :args (s/cat :cmp fn? :heap ::heap)
  :ret ::heap)

(defn delete-min* [cmp heap]
  (if (empty? heap)
    heap
    (let [get-min (fn get-min [[node & nodes]]
                    (if (empty? nodes)
                      [node nil]
                      (let [[node' nodes'] (get-min nodes)]
                        (if (leq cmp (element node) (element node'))
                          [node nodes]
                          [node' (cons node nodes')]))))

          [min-node remaining] (get-min heap)]
      (merge-heap* cmp (reverse (children min-node)) remaining))))

(s/fdef make-heap
  :args (s/cat :compare fn?)
  :ret ::heap)

(declare -lazy-vals)

(defprotocol IHeap
  (-find-min [heap])
  (-delete-min [heap])
  (-insert [heap x])
  (-heap-sort [heap]))

(s/fdef insert
  :args (s/cat :cmp fn? :tree ::heap :x any?)
  :ret ::heap)

(deftype HeapWrapper [nodes cmp cnt]
  clojure.lang.Counted
  (count [_] cnt)

  clojure.lang.Seqable
  (seq [_]
    (letfn [(ls [[n & rst]]
              (when n
                (lazy-cat [(element n)]
                          (ls (children n))
                          (ls rst))))]
      (ls nodes)))

  clojure.lang.IPersistentCollection
  (cons [heap x] (-insert heap x))
  (empty [_heap] (HeapWrapper. () cmp 0))
  (equiv [heap o] (and (instance? HeapWrapper o)
                       (= cnt (.-cnt ^HeapWrapper o))
                       (= (-heap-sort heap) (-heap-sort o))))

  IHeap
  (-find-min [_]
    (find-min* cmp nodes))
  (-delete-min [_]
    (HeapWrapper. (delete-min* cmp nodes) cmp (dec cnt)))
  (-insert [_ x]
    (HeapWrapper. (insert-tree* cmp nodes (Node. 0 x ()))
                  cmp
                  (inc cnt)))
  (-heap-sort [_]
    (loop [sorted []
           nodes nodes]
      (if-let [x (find-min* cmp nodes)]
        (recur (conj sorted x) (delete-min* cmp nodes))
        sorted))))

(defn insert [heap x] (-insert heap x))
(defn find-min [heap] (-find-min heap))
(defn delete-min [heap] (-delete-min heap))
(defn heap-sort [heap] (-heap-sort heap))

(defn make-heap [compare-fn]
  (HeapWrapper. () compare-fn 0))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument `[insert-tree* delete-min* find-min*])
  (stest/unstrument `[insert-tree* delete-min* find-min*])
  (find-min
    (delete-min
      (reduce insert
              (make-heap compare)
              (range 4))))

  (import java.util.PriorityQueue)
  (def integers #(shuffle (range 1000)))
  (time
    (let [q (PriorityQueue. 500 compare)]
      (doseq [i (integers)]
        (.offer q i))
      (.poll q)))

  (time
    (do
      (let [q (PriorityQueue. 500 compare)]
        (doseq [i (integers)]
          (.offer q i))
        (loop [result []
               q      q]
          (if-let [x (.poll q)]
            (recur (conj result x) q)
            result)))
      nil))
  (find-min
    (reduce insert
            (make-heap compare)
            (take 5000 (integers))))

  (time
    (find-min (transduce (map identity)
                         (fn queueueue
                           ([] (make-heap compare))
                           ([result] result)
                           ([result x] (insert result x)))
                         (integers))))
  (let [sorted (sort (integers))]
    (time
      (= sorted
         (heap-sort
           (transduce (map identity)
                      (fn queueueue
                        ([] (make-heap compare))
                        ([result] result)
                        ([result x] (insert result x)))
                      (integers))))))

  (let [input (shuffle (range 1e5))]
    (time
      (transduce (map identity)
                 (let [threshold 10]
                   (fn
                     ([] (make-heap compare))
                     ([heap] (heap-sort heap))
                     ([heap e] (cond (< (count heap) threshold)
                                     (insert heap e)

                                     (> e (find-min heap))
                                     (insert (delete-min heap) e)

                                     :else
                                     heap))))
                 input)))

  (let [input (shuffle (range 1e5))]
    (time (take 10 (sort input))))
  )

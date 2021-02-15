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
  (cmp [_])
  (link [_ _]))

(defn leq [cmp x y]
  (<= (cmp x y) 0))

(defn- app [x y]
  (with-meta (cons x y) (meta y)))

(deftype Node [rnk e chdren cmp]
  INode
  (rank [_] rnk)
  (element [_] e)
  (children [_] chdren)
  (cmp [_] cmp)
  (link [n1 n2]
    (if (leq cmp e (element n2))
      (Node. (inc rnk) e            (app n2 chdren)        cmp)
      (Node. (inc rnk) (element n2) (app n1 (children n2)) cmp))))

(declare heap?)

(defn node? [node]
  (and (instance? Node node)
       (every? heap? (children node))))

(defn heap? [x]
  (and (sequential? x)
       (contains? (meta x) ::compare)
       (every? node? x)))

(s/def ::heap heap?)
(s/def ::node node?)

(s/fdef insert-tree
  :args (s/cat :node ::node :heap ::heap))

(defn- insert-tree [node heap]
  (if (empty? heap)
    (with-meta (list node) {::compare (cmp node)})
    (let [[t2 & rst] heap]
      (if (< (rank node) (rank t2))
        (app node heap)
        (recur (link node t2) rst)))))

(s/fdef merge-heap
  :args (s/cat :tree1 ::heap :tree2 ::heap)
  :ret ::heap)

(defn- merge-heap [[t1 & ts1 :as tree1] [t2 & ts2 :as tree2]]
  (cond (empty? tree2)          tree1
        (empty? tree1)          tree2
        (< (rank t1) (rank t2)) (app t1 (merge-heap ts1 tree2))
        (> (rank t1) (rank t2)) (app t2 (merge-heap tree1 ts2))
        :else                   (insert-tree (link t1 t2) (merge-heap ts1 ts2))))

(s/fdef find-min
  :args (s/cat :heap ::heap)
  :ret ::heap)

(defn find-min [[node & nodes :as heap]]
  (cond (empty? heap)  nil
        (empty? nodes) (element node)
        :else          (let [element (element node)
                             min' (find-min nodes)]
                         (if (leq (cmp node) element min') element min'))))

(s/fdef delete-min
  :args (s/cat :ts ::heap)
  :ret ::heap)

(defn delete-min [heap]
  (if (empty? heap)
    heap
    (let [get-min (fn get-min [[node & nodes]]
                    (if (empty? nodes)
                      [node nil]
                      (let [[node' nodes'] (get-min nodes)]
                        (if (leq (cmp node) (element node) (element node'))
                          [node nodes]
                          [node' (app node nodes')]))))

          [min-node remaining] (get-min heap)]
      (merge-heap (with-meta (reverse (children min-node)) {::compare (cmp min-node)})
                  remaining))))

(s/fdef make-heap
  :args (s/cat :compare fn?)
  :ret ::heap)

(defn make-heap [compare]
  ^{::compare compare} ())

(s/fdef insert
  :args (s/cat :x any? :tree ::heap)
  :ret ::heap)

(defn insert [x tree]
  (let [cmp (get (meta tree) ::compare)]
    (insert-tree (Node. 0 x (with-meta () {::compare cmp}) cmp) tree)))


(comment
  #{clojure.lang.Indexed
    clojure.lang.Associative
    clojure.lang.IKVReduce
    clojure.lang.ILookup
    clojure.lang.Sequential
    java.lang.Runnable
    clojure.lang.Seqable
    clojure.lang.IObj
    java.util.List
    java.io.Serializable
    clojure.lang.IFn
    clojure.lang.IPersistentStack
    java.lang.Comparable
    clojure.lang.Reversible
    clojure.lang.AFn
    java.lang.Iterable
    clojure.lang.IHashEq
    clojure.lang.IEditableCollection
    java.util.Collection
    clojure.lang.IMeta
    clojure.lang.IPersistentCollection
    java.util.concurrent.Callable
    java.lang.Object
    java.util.RandomAccess
    clojure.lang.APersistentVector
    clojure.lang.IReduceInit
    clojure.lang.IReduce
    clojure.lang.Counted
    clojure.lang.IPersistentVector}

  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument `[insert insert-tree make-heap delete-min find-min])
  (stest/unstrument `[insert insert-tree make-heap delete-min find-min])
  (insert 3 ())
  (insert 3 (make-heap compare))
  (heap? (make-heap compare))
  (clojure.repl/doc insert)
  (clojure.repl/dir stest)

  (heap? ())
  (heap? ^{::compare compare} ())
  (find-min
    (delete-min
      (reduce (fn [tree x] (insert-tree (Node. 0 x (with-meta () {::compare compare}) compare)
                                        tree))
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
    (reduce (fn [heap x] (insert x heap))
            (make-heap compare)
            (take 5000 (integers))))

  (defn heap-sort [heap]
    (loop [result []
           heap heap]
      (if-let [x (find-min heap)]
        (recur (conj result x) (delete-min heap))
        result)))
  (time
    (find-min (transduce (map identity)
                         (fn queueueue
                           ([] (make-heap compare))
                           ([result] result)
                           ([result x] (insert x result)))
                         (integers))))
  (let [sorted (sort (integers))]
    (time
      (= sorted
         (heap-sort
           (transduce (map identity)
                      (fn queueueue
                        ([] (make-heap compare))
                        ([result] result)
                        ([result x] (insert x result)))
                      (integers))))))

  (defn count-heap
    "Terribly slow implementation. Will need to keep this information
  around if we want to have a way to count."
    [heap]
    (letfn [(count-node [node]
              (+ 1 (count-heap (children node))))]
      (reduce + 0 (map count-node heap))))

  (time
    (transduce (map identity)
               (let [threshold 10
                     c         (volatile! 0)]
                 (fn
                   ([] (make-heap compare))
                   ([heap] (heap-sort heap))
                   ([heap e] (cond (< @c #_(count-heap heap) threshold)
                                   (do (vswap! c inc) (insert e heap))

                                   (> e (find-min heap))
                                   (insert e (delete-min heap))
                                   #_(->> heap delete-min (insert e))

                                   :else
                                   heap))))
               (shuffle (range 1e5))))
  )

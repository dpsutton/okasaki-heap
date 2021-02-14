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

(declare heap?)
(declare node?)

(defprotocol Nod
  (rank [_])
  (root [_])
  (cmp [_]))

(defn leq [cmp x y]
  (<= (cmp x y) 0))

(deftype Node [rnk e children cmp]
  Nod
  (rank [_] rnk)
  (root [_] e)
  (cmp [_] cmp))

(defn- app [x y]
  (with-meta (cons x y) (meta y)))

(defn link [n1 n2]
  (if (leq (cmp n1) (root n1) (root n2))
    (Node. (inc (rank n1)) (root n1) (app n2 (.-children n1)) (cmp n1))
    (Node. (inc (rank n1)) (root n2) (app n1 (.-children n2)) (cmp n1))))

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
        (empty? nodes) (root node)
        :else          (let [root (root node)
                             min' (find-min nodes)]
                         (if (leq (cmp node) root min') root min'))))

(s/fdef delete-min
  :args (s/cat :ts ::heap)
  :ret ::heap)

(defn delete-min [heap]
  (if (empty? heap)
    nil
    (let [get-min (fn get-min [[node & nodes]]
                    (if (empty? nodes)
                      [node nil]
                      (let [[node' nodes'] (get-min nodes)]
                        (if (leq (cmp node) (root node) (root node'))
                          [node nodes]
                          [node' (app node nodes')]))))

          [min-node remaining] (get-min heap)]
      (merge-heap (with-meta (reverse (.-children min-node)) {::compare (cmp min-node)})
                  remaining))))

(s/fdef make-heanp
  :args (s/cat :compare fn?))

(defn make-heap [compare]
  ^{::compare compare} ())

(s/fdef insert
  :args (s/cat :x any? :tree ::heap)
  :ret heap?)

(defn insert [x tree]
  (let [cmp (get (meta tree) ::compare)]
    (insert-tree (Node. 0 x (with-meta () {::compare compare}) cmp) tree)))

(defn node? [node]
  (and (instance? Node node)
       (every? heap? (.-children node))))

(defn heap? [x]
  (and (sequential? x)
       (contains? (meta x) ::compare)
       (every? node? x)))

(comment

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
  (def integers #(shuffle (range 100)))
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
  (time
    (= (sort (integers))
       (heap-sort
         (transduce (map identity)
                    (fn queueueue
                      ([] (make-heap compare))
                      ([result] result)
                      ([result x] (insert x result)))
                    (integers)))))

  )

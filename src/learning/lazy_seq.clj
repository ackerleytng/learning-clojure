(ns learning.lazy-seq
  (:gen-class))

;; What is lazy-seq?
;; It is a macro that takes a body of expressions
;;         and yields a seqable object
;; This body of expressions must return an ISeq or nil
;; The seqable object will invoke the body when seq is called on it
;;   and memoize the result of invoking the body

;; How to use lazy-seq?
;; Think of lazy-seq as a way of delaying computation of the body wrapped in lazy-seq
;;   The body must return an ISeq or nil

;; Quick way to think about it
;; 1. Write a normal recursive function that builds up a list with cons

(defn positive-numbers-normal
  [n]
  (cons n (positive-numbers-normal (inc n))))

;; 2. Wrap the part you want to delay with lazy-seq
;; 2a. I'm delaying the entire computation

(defn positive-numbers
  [n]
  (lazy-seq (cons n (positive-numbers (inc n)))))

;; 2b. I'm delaying the computation of the next number

(defn positive-numbers-delay-next
  [n]
  (cons n (lazy-seq (positive-numbers-delay-next (inc n)))))

;; You can see the difference if computation takes a long time

(defn slow-computation
  [n]
  (Thread/sleep 1000)
  n)

(defn slow-positive-numbers
  [n]
  (lazy-seq (cons (slow-computation n) (slow-positive-numbers (inc n)))))

(defn slow-positive-numbers-delay-next
  [n]
  (cons (slow-computation n) (lazy-seq (slow-positive-numbers (inc n)))))

(def slower-since-computed-at-declaration (slow-positive-numbers-delay-next 1))
(def faster-since-computation-is-delayed (slow-positive-numbers 1))

;; The body wrapped with lazy-seq should return nil if the sequence should stop

(defn simple-range
  [i limit]
  (lazy-seq (when (< i limit)
              (cons i (simple-range (inc i) limit)))))

;; Practice

;; 1. Squares

(defn squares
  [n]
  (cons (* n n) (lazy-seq (squares (inc n)))))

(take 10 (squares 1))

;; 2. Fibonacci

(defn fibonacci-series
  ([]
   (fibonacci-series 0 1))
  ([a b]
   (cons b (lazy-seq (fibonacci-series b (+ a b))))))

(take 10 (fibonacci-series))

;; 3. Quicksort

(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(defn qsort
  "How you would implement quicksort normally"
  [[pivot & xs]]
  (if pivot
    (let [smaller? #(< % pivot)]
      (into (conj
             (qsort (filter smaller? xs))
             pivot)
            (qsort (remove smaller? xs))))
    []))

(defn sort-parts [work]
  "Implementing quicksort lazily.
   work is a list of either
    + unsorted lists of numbers
    + pivots (numbers)"
  (lazy-seq
   (loop [[part & parts] work]
     ;; Loop over all the work
     ;; part will always be a list, since at the list*,
     ;;   + the first item is always a list (even if it is an empty list)
     ;;   + after the pivot is removed, the bigger numbers after the pivot form a list too
     (if-let [[pivot & xs] (seq part)]
       ;; If it is a list
       (let [smaller? #(< % pivot)]
         ;; Then we can find the numbers smaller than the pivot
         (recur (list*
                 ;; And put those smaller numbers ahead of the pivot
                 (filter smaller? xs)
                 pivot
                 ;; And the bigger numbers after of the pivot
                 (remove smaller? xs)
                 ;; Ahead of all the other parts
                 parts)))
       ;; If it is an empty list, then there is nothing smaller than the pivot
       ;;   => the pivot is the smallest
       (when-let [[x & parts] parts]
         ;; So we can take the smallest and yield it
         (cons x (sort-parts parts)))))))

(qsort (rand-ints 10))
(sort-parts [(rand-ints 10)])

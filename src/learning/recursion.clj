(ns learning.recursion
  (:gen-class))

;; Mundane recursion

(def simple-metric {:meter 1
                    :km 1000
                    :cm 1/100
                    :mm [1/10 :cm]})


(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(convert {:bit 1 :byte 8 :nybble [1/2 :byte]} [32 :nybble])

;; Preventing stack overflows with trampoline

(defn elevator [commands]
  (letfn
      [(ff-open [[_ & r]]
         "When the elevator is open on the first floor
          it can either close or be done"
         #(case _
            :close (ff-closed r)
            :done true
            false))
       (ff-closed [[_ & r]]
         "When the elevator is closed on the first floor
          it can either open or go up"
         #(case _
            :open (ff-open r)
            :up (sf-closed r)
            false))
       (sf-closed [[_ & r]]
         "When the elevator is closed on the second floor
          it can either open or go down"
         #(case _
            :open (sf-open r)
            :down (ff-closed r)
            false))
       (sf-open [[_ & r]]
         "When the elevator is open on the second floor
          it can either close or be done"
         #(case _
            :close (sf-closed r)
            :done true
            false))]
    ;; Each function returns a function so that trampoline can manage the process of self-calls
    (trampoline ff-open commands)))

(elevator [:close :open :close :up :open :open :done])
(elevator [:close :up :open :close :down :open :done])

;; Continuation-passing style

(defn factorial-cps
  [n continuation-function]
  (letfn [(cont [v] (continuation-function (* v n)))] ;; Next
    (if (zero? n) ;; Accept
      (continuation-function 1) ;; Return
      (recur (dec n) cont))))

(defn factorial [n]
  (factorial-cps n identity))

(factorial 3)

(defn mk-cps [accept? kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k ((partial kont v) n)))]
         (if (accept? n)
           (k 1)
           (recur (dec n) cont))))
     n kend)))

(def fac-cps (mk-cps zero? identity *))

(fac-cps 10)

(def tri-cps (mk-cps (partial = 1) identity +))

(tri-cps 10)

;; A* pathfinding

;; A* is a best-first pathfinding algorithm that maintains a set of candidate paths
;;   through a world with the purpose of finding the least-difficult path to some goal

(def world [[  1   1   1   1   1]
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

(defn valid-pos
  [size pos]
  (every? #(and (>= % 0) (< % size)) pos))

(defn neighbors
  [size [r c]]
  (filter (partial valid-pos size) [[(inc r) c]
                                    [r (inc c)]
                                    [(dec r) c]
                                    [r (dec c)]]))

(neighbors 5 [0 0])

(defn estimate-cost [step-cost-est size r c]
  (* step-cost-est
     (- (+ size size) r c 2)))

(estimate-cost 900 5 0 0)
(estimate-cost 900 5 4 4)

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(path-cost 900 {:cost 1})

(defn total-cost [newcost step-cost-est size r c]
  (+ newcost
     (estimate-cost step-cost-est size r c)))

(total-cost 0 900 5 0 0)

;; If you're 1 step away with a current cost of 1000, the estimated cost is 900

(total-cost 1000 900 5 3 4)

(defn min-by
  [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))

(min-by :cost [{:cost 100}
               {:cost 36}
               {:cost 9}])

(defn astar
  [start-pos step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [;; Number of work items the algorithm has worked on so far
           steps 0
           ;; State is kept in this function using a vector of vectors
           ;;   The vector of vectors contains the cheapest known route to this position
           ;;   stored as {:cost <cost of this route>
           ;;              :route [positions leading up to this position (the route so far)]}
           routes (vec (repeat size (vec (repeat size nil))))
           ;; todo list in a vector of [<estimated cost to destination> <position to be explored>]
           ;;   estimated cost to destination is computed by summing
           ;;     + the known cheapest cost to get here
           ;;     + estimated cost to get to the end
           ;;   The todo list is only lengthened if astar finds a cheaper route to this position,
           ;;     hence preventing infinite loops.
           work-todo (sorted-set [0 start-pos])]
      (if (empty? work-todo)
        ;; Double peek gets the last route (route to [4 4])
        [(peek (peek routes)) :steps steps]
        (let [;; Pick off the first item on the todo list
              [_ pos :as work-item] (first work-todo)
              ;; Remove this from the todo list in preparation for next time
              rest-work-todo (disj work-todo work-item)
              ;; Get neighboring positions
              nbr-pos (neighbors size pos)
              ;; Find the cheapest way to get to any of the neighbors
              cheapest-nbr (min-by :cost
                                   ;; Find the costs of getting to the neighbors,
                                   ;;   keeping the explored ones
                                   (keep #(get-in routes %) nbr-pos))
              ;; The cheapest cost of getting to this position is the sum of
              ;;   + the cell cost for this position
              ;;   + the cheapest cost of getting to neighbor
              newcost (path-cost (get-in cell-costs pos) cheapest-nbr)
              ;; The previously-known cheapest way to get to this position
              oldcost (:cost (get-in routes pos))]
          (clojure.pprint/pprint [pos newcost oldcost work-todo])
          (if (and oldcost (>= newcost oldcost))
            ;; If there was a previously known cost and the new cost is higher,
            ;;   retain the old information
            (recur (inc steps) routes rest-work-todo)
            ;; Since the new cost is lower...
            (recur (inc steps)
                   ;; Replace existing information (if it exists) with new information
                   (assoc-in routes pos {:cost newcost
                                         :route (conj (:route cheapest-nbr [])
                                                      pos)})
                   ;; Estimate costs to get to neighbors and put neighbors on work list
                   (into rest-work-todo
                         (map (fn [[r c :as w]]
                                [(total-cost newcost step-est size r c) w])
                              nbr-pos)))))))))

;; Preventing an infinite loop: taking [0 0] as an example.

;; When the algorithm is working on [0 1], it will add [0 0] as a new work item
;;   when [0 0] is checked again, the algorithm will look up the neighbors of [0 0],
;;   trying to find the cheapest path to get to [0 0]. It will compute a newcost, which will
;;   inevitably be higher than the existing cost to get to that position,
;;   hence the todo list does not grow

(def shrubbery-world [[  1   1   1   2   1]
                      [  1   1   1 999   1]
                      [  1   1   1 999   1]
                      [  1   1   1 999   1]
                      [  1   1   1   1   1]])

(astar [0 0] 900 shrubbery-world)

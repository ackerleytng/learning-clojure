(ns learning.macros
  (:import [java.io BufferedReader InputStreamReader]
           [java.net URL]))

(defn contextual-eval
  [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defmacro do-until
  [& clauses]
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(defmacro unless-manual
  [cond & body]
  (list 'if (cons 'clojure.core/not (list cond))
        (cons 'do body)))

(defmacro unless
  [cond & body]
  `(if (not ~cond)
     (do ~@body)))

#_(macroexpand-1 '(unless false (prn :dd)))
#_(macroexpand-1 '(unless-manual false (prn :dd)))

(defmacro def-watched
  [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

#_(def-watched x (* 12 12))
#_(def x 0)

(defmacro domain
  [name & body]
  `{:tag :domain
    :attrs {:name (str '~name)}
    :content [~@body]})

(declare handle-things)

(defmacro grouping
  [name & body]
  `{:tag :grouping
    :attrs {:name (str '~name)}
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things
  [things]
  (for [t things]
    {:tag :thing
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-props
  [props]
  (when props
    {:tag :properties
     :attrs nil
     :content (apply vector (for [p props]
                              {:tag :property
                               :attrs {:name (str (first p))}
                               :content nil}))}))

(defn grok-attrs
  [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock human")
                    (Man (isa Human)
                         "A man, baby"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yet elusive creature"
                     [eats-goats?]))))

(defmacro awhen
  [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(defmacro my-if-let
  [[binding expression] if-block else-block]
  `(let [~'b ~expression]
     (if ~'b
       ~if-block
       ~else-block)))

(defmacro my-if-let-2
  [binding if-block else-block]
  (let [name (binding 0)]
    `(let ~binding
       (if ~name
         ~if-block
         ~else-block))))

(defmacro my-if-let-3
  [binding if-block else-block]
  `(let ~binding
     (if ~(binding 0)
       ~if-block
       ~else-block)))

(macroexpand-1 '(my-if-let-3 [c 1]
                  (inc c)
                  :nay))

(defmacro with-resource
  [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))

(defn joc-www []
  (-> "http://www.google.com"
      URL.
      .openStream
      InputStreamReader.
      BufferedReader.))

#_(let [stream (joc-www)]
    (with-open [page stream]
      (println (.readLine page))
      (print "The stream will now close..."))
    (println "but let's read from it anyway.")
    (.readLine stream))

#_(Macros returning functions)

(declare collect-bodies)

(defmacro contract
  [name & forms]
  (list* `fn name (collect-bodies forms)))

(fn doubler
  ([f x]
   {:post [(= (* 2 x) %)]
    :pre [(pos? x)]}
   (f x)))

(declare build-contract)

(defn collect-bodies
  [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract
  [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) 'require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) 'ensure)
                    (assoc {} :post (vec (rest con)))
                    :else
                    (throw (Exception. (str "Unknown tag "
                                            (first con)))))))
     (list* 'f args))))

(def doubler-contract
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))))

#_(macroexpand-1 '(contract doubler
                            [x]
                            (require
                             (pos? x))
                            (ensure
                             (= (* 2 x) %))))

(def times2 (partial doubler-contract #(* 2 %)))

(times2 9)

(def times3 (partial doubler-contract #(* 3 %)))

#_(times3 9)

(def doubler-contract-1
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))
            [x y]
            (require
             (pos? x)
             (pos? y))
            (ensure
             (= (* 2 (+ x y)) %))))

(macroexpand-1 '(contract doubler
                          [x]
                          (require
                           (pos? x))
                          (ensure
                           (= (* 2 x) %))
                          [x y]
                          (require
                           (pos? x)
                           (pos? y))
                          (ensure
                           (= (* 2 (+ x y)) %))))

((partial doubler-contract-1 #(* 2 (+ %1 %2))) 2 3)

((partial doubler-contract-1 #(+ %1 %2 %1 %2)) 2 3)

#_((partial doubler-contract-1 #(* 3 (+ %1 %2))) 2 3)

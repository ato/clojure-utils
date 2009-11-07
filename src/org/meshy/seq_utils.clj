(ns org.meshy.seq-utils
  (:use [clojure.contrib.generic.functor :only [fmap]]))

(defn map-reduce-by
  "Returns a hash map where elements of coll are mapped by map-fn
  and then reduced by reduce-fn.  The map is keyed by the result 
  of group-fn on the original element.  If specified, val is the 
  initial value of the reduce operation."
  ([group-fn map-fn reduce-fn coll]     
     (reduce 
      (fn [m x]
        (let [group (group-fn x)
              mx (map-fn x)
              pair (find m group)]
          (assoc m group (if pair (reduce-fn (val pair) mx) mx))))
      {} coll))
  ([group-fn map-fn reduce-fn val coll]
     (println "group" group-fn "map" map-fn "reduce" reduce-fn "val"
              val "coll" coll)
     (reduce 
      (fn [m x]
        (let [group (group-fn x)]
          (assoc m group (reduce-fn (get m group val) (map-fn x)))))
      {} coll)))

(defn reduce-by
  "Like reduce but returns a map keyed by the result of group-fn on
  each element of coll."
  ([group-fn reduce-fn coll] 
     (map-reduce-by group-fn identity reduce-fn coll))
  ([group-fn reduce-fn val coll]
     (map-reduce-by group-fn identity reduce-fn val coll)))

(defn group-by
  "Returns a hash map of the elements of coll keyed by the result of
  group-fn on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  ([group-fn coll]        (reduce-by group-fn conj [] coll))
  ([group-fn map-fn coll] (map-reduce-by group-fn map-fn conj [] coll)))

(defn freq-by
  "Applies grouper to each item in coll and returns a map from the distinct
  results to the number of times they occur."
  [grouper coll]
  (map-reduce-by grouper (constantly 1) + coll))

(defn freq
  "Returns a map from distinct items in coll to the number of times
   they appear."
  [coll]
  (freq-by identity coll))

(defn sum
  "Calculates the sum of the elements of coll."
  [coll]
  (reduce + coll))

(defn- grouper [f] 
  (fn
    ([group-fn map-fn coll] (map-reduce-by group-fn map-fn f coll))
    ([group-fn coll]        (reduce-by group-fn f coll))))

(def sum-by (grouper +))
(def min-by (grouper min))
(def max-by (grouper max))

(defn stats-by
  "Calculates a map from elements of coll grouped by group-fn 
  to [frequency mean standard-deviation]."
  ([group-fn map-fn coll] 
     (->> coll
          (map-reduce-by group-fn
                         #(let [x (map-fn %)] [1 x (* x x)])
                         (partial map +)
                         [0 0 0])
          (fmap (fn [[n sum sum-sq]]
                  [n (quot sum n) (Math/sqrt (- (* sum sum) 
                                                (quot sum-sq n)))])))))


(defn partition-when
  "Partitions coll starting a new sequence whenever pred is true.
  Returns a lazy sequence of lazy sequences.

  Example: (partition-when even? [2 4 3 5 7 8 10])
            => ((2) (4 3 5 7) (8) (10))"
  [pred coll]
  (lazy-seq
    (when-let [[x & xs] (seq coll)]
      (let [[xs ys] (split-with (complement pred) xs)]
        (cons (cons x xs) (partition-when pred ys))))))

(comment
  ;;
  ;; Some examples
  ;;

  (def coin-flips [:h :h :t :h :t :h :h :t])

  (def products #{{:product :tv    :cost 400 :weight 25}
                  {:product :paper :cost 20  :weight 1}
                  {:product :cd    :cost 5   :weight 1}})

  (def product-index (zipmap (map :product products) products))

  (def owners #{{:name "Dorris" :product :tv :count 5}
                {:name "Joe"    :product :cd :count 2}
                {:name "Dorris" :product :cd :count 2}
                {:name "Joe"    :product :tv :count 2}
                {:name "Carol"  :product :tv :count 2}
                {:name "Louise" :product :cd :count 2}})

  (group-by :weight products)
  ;; => {25 [{:product :tv, :cost 400, :weight 25}],
  ;;     1  [{:product :cd, :cost 5, :weight 1}
  ;;         {:product :paper, :cost 20, :weight 1}]}

  (group-by :weight :product products)
  ;; => {25 [:tv], 1 [:cd :paper]}

  (freq coin-flips)
  ;; => {:t 3, :h 5}

  (freq-by :product owners)
  ;; => {:tv 3, :cd 5}

  (sum-by (juxt :product :name)
          #(* (:count %)
              (-> % :product product-index :cost))
          owners)
  ;; => {[:tv "Dorris"] 2000
  ;;     [:cd "Dorris"]   10
  ;;     [:tv "Carol"]   800
  ;;     [:tv "Joe"]     800
  ;;     [:cd "Joe"]      10
  ;;     [:cd "Louise"]   10}

  (stats-by :weight :cost products)
  ;; => {25 [1 400 0.0], 
  ;;      1 [2 12 20.322401432901575]}

  )

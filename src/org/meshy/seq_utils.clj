(ns org.meshy.seq-utils)

(defn reduce-by
  "Like reduce but returns a map where keys are values of 
  (grouper x)."
  ([grouper f coll]     
     (reduce (fn [m x]
               (let [group (grouper x)
                     pair (find m group)]
                 (assoc m group (if pair (f (val pair) x) x))))
             {} coll))
  ([grouper f val coll]
     (reduce (fn [m x]
               (let [group (grouper x)]
                 (assoc m group (f (get m group val) x))))
             {} coll)))

(defn group-by
  "Returns a hash map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  [f coll]
  (reduce-by f conj [] coll))

(defn freq-by
  "Applies grouper to each item in coll and returns a map from the distinct
  results to the number of times they occur."
  [grouper coll]
  (reduce-by grouper (fn [x _] (inc x)) 0 coll))

(defn freq
  "Returns a map from distinct items in coll to the number of times
  they appear."
  [coll]
  (freq-by identity coll))

(defn sum-by
  "Returns a map of the sum of (summer x) for x in coll grouped by
  (grouper x)."
  ([grouper summer coll]
     (reduce-by grouper (fn [sum x] (+ sum (summer x))) 0 coll)))


(comment
  (let [products {:tv    {:cost 400 :weight 25}
                  :paper {:cost 20  :weight 1}}
        owners #{{:name "dorris" :sale 5 :product :tv}
                 {:name "joe" :sale 2 :product :paper}
                 {:name "dorris" :sale 2 :product :paper}
                 {:name "joe" :sale 2 :product :tv}
                 {:name "carol" :sale 2 :product :tv}
                 {:name "louise" :sale 2 :product :tv}}]
    (sum-by #(vector (:product %) (:name %)) 
            (comp :cost products :product) owners))

)

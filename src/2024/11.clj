(require '[clojure.string :as str]
         '[clojure.math :as math])

(defn count-digits [n]
  (count (str n)))

(defn split-number [n]
  (let [len (count-digits n)
        half (quot len 2)
        div (long (math/pow 10 (- len half)))]
    [(quot n div) (rem n div)]))

(def transform-stone
  (memoize
   (fn [n]
     (cond
       (zero? n) [1]
       (even? (count-digits n)) (split-number n)
       :else [(* n 2024)]))))

(defn transform-stones [stones]
  (reduce-kv
   (fn [acc n count]
     (reduce (fn [m new-n]
               (update m new-n (fnil + 0) count))
             acc
             (transform-stone n)))
   {}
   stones))

(defn count-stones [stones n]
  (loop [stones (frequencies stones)
         blinks 0]
    (if (= blinks n)
      (reduce + (vals stones))
      (recur (transform-stones stones) (inc blinks)))))

(let [data (-> (slurp "inputs/2024/11.txt")
               (str/split #"\s+")
               (as-> nums (mapv parse-long nums)))]
  [(count-stones data 25) (count-stones data 75)])

(require '[clojure.string :as str])

(defn step [secret]
  (-> secret
      (#(bit-and (bit-xor % (bit-shift-left % 6)) 16777215))
      (#(bit-and (bit-xor % (bit-shift-right % 5)) 16777215))
      (#(bit-and (bit-xor % (bit-shift-left % 11)) 16777215))))

(defn bananas [secret]
  (letfn [(keep-first [m [p price]] (if (contains? m p) m (assoc m p price)))]
    (->> (iterate step secret) (take 2001) (map #(mod % 10))
         (#(reduce keep-first {} (map vector (partition 4 1 (map - (rest %) %)) (drop 4 %)))))))

(let [secrets (->> "inputs/2024/22.txt" slurp str/split-lines (mapv parse-long))]
  [(time (->> secrets (pmap #(nth (iterate step %) 2000)) (reduce +)))
   (time (->> secrets (partition-all (/ (count secrets) 10))
              (pmap #(apply merge-with + (map bananas %))) (apply merge-with +) vals (apply max)))])

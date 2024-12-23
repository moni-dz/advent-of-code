(ns mmxxiv.xxii
  (:require [clojure.string :as str]
            [lib.core :as lib]))

(defn step [secret]
  (-> secret
      (#(bit-and (bit-xor % (bit-shift-left % 6)) 16777215))
      (#(bit-and (bit-xor % (bit-shift-right % 5)) 16777215))
      (#(bit-and (bit-xor % (bit-shift-left % 11)) 16777215))))

(defn bananas [secret]
  (letfn [(diff->key [diffs] (reduce #(+ (* %1 20) (+ %2 10)) 0 diffs))
          (keep-first [m [p price]] (if (contains? m p) m (assoc m p price)))]
    (->> (take 2000 (map #(mod % 10) (iterate step secret)))
         (#(reduce keep-first {} (map vector (map diff->key (sequence (lib/sliding 4) (map - (rest %) %))) (drop 4 %)))))))

(let [secrets (->> "inputs/2024/22.txt" slurp str/split-lines (mapv parse-long))]
  [(time (->> secrets (pmap #(nth (iterate step %) 2000)) (reduce +)))
   (time (->> secrets (partition-all (/ (count secrets) 10))
              (pmap #(apply merge-with + (map bananas %))) (apply merge-with +) vals (apply max)))])

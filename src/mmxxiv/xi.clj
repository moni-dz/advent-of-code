(ns mmxxiv.xi
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn split-number [n]
  (let [len (count (str n))
        half (quot len 2)
        div (long (math/pow 10 (- len half)))]
    [(quot n div) (rem n div)]))

(def transform
  (memoize #(cond (zero? %) [1]
                  (even? (count (str %))) (split-number %)
                  :else [(* % 2024)])))

(defn transforms [stones]
  (let [stone (mapcat (fn [[n count]] (map #(vector % count) (transform n))))]
    (transduce stone (fn ([] {}) ([m] m) ([m [k v]] (update m k (fnil + 0) v))) stones)))

(defn stones [stones n]
  (transduce (comp (drop n) (take 1) (mapcat vals)) + 0
             (->> stones frequencies (iterate transforms))))

(let [data (mapv parse-long (str/split (slurp "inputs/2024/11.txt") #"\s+"))]
  (mapv #(time (stones data %)) [25 75]))

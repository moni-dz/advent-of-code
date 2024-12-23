(ns mmxxiv.vii
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn multiplier [n]
  (long (math/pow 10 (int (inc (math/log10 n))))))

(def multiplier! (memoize multiplier))

(defn apply-op [a b op]
  (case op
    \+ (+ a b)
    \* (* a b)
    \| (+ (* a (multiplier! b)) b)))

(defn dfs [exprs-count exprs target ops index value]
  (cond
    (> value target) nil
    (= index (dec exprs-count))
    (when (= value target)
      true)
    :else
    (some (fn [op]
            (let [next-number (nth exprs (inc index))
                  new-value (apply-op value next-number op)]
              (dfs exprs-count exprs target ops (inc index) new-value)))
          ops)))

(defn solve [ops [target exprs]]
  (let [exprs-count (count exprs)]
    (when (dfs exprs-count exprs target ops 0 (first exprs))
      target)))

(defn solve! [data ops]
  (->> data
       (pmap #(solve ops %))
       (filter some?)
       (reduce +)))

(let [data (->> (slurp "inputs/2024/7.txt")
                str/split-lines
                (map #(let [[n rest] (str/split % #":\s+")
                            target (parse-long n)
                            exprs (vec (map parse-long (str/split rest #"\s+")))]
                        [target exprs])))]

  [(time (solve! data #{\+ \*}))
   (time (solve! data #{\+ \* \|}))])

(ns mmxxiv.xxv
  (:require [clojure.string :as str]))

(letfn [(parse [s] (->> s str/split-lines (map #(mapv {\# 1 \. 0} %)) (apply map +)))]
  (->> "inputs/2024/25.txt" slurp str/trim (#(str/split % #"\n\n"))
       ((juxt #(filter (comp #{\.} first) %) #(filter (comp #{\#} first) %)))
       (map #(map parse %))
       ((fn [[ks ls]]
          (->> ks
               (mapcat #(filter (fn [lock] (every? (fn [[x y]] (<= (+ x y) 7)) (map vector lock %))) ls))
               count)))))

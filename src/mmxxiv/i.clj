(ns mmxxiv.i
  (:require [clojure.string :as str]))

(let [[xs ys] (->> "inputs/2024/1.txt" slurp str/split-lines
                   (mapv #(mapv parse-long (str/split % #"\s+"))) (apply map vector))]
  [(reduce + (map #(abs (- %1 %2)) (sort xs) (sort ys)))
   (reduce + (map #(* % (get (frequencies ys) % 0)) xs))])

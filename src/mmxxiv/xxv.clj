(ns mmxxiv.xxv
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(->> "inputs/2024/25.txt"
     slurp
     (#(str/split % #"\n\n"))
     (map #(str/replace % "\n" ""))
     ((juxt #(filter (comp #{\#} first) %) #(filter (comp #{\.} first) %)))
     (apply combo/cartesian-product)
     (filter (fn [[l k]] (not-any? #(= [\# \#] %) (map vector l k))))
     count)

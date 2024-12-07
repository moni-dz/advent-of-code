(require '[clojure.string :as str])

(let
 [data (->> (slurp "inputs/2024/1.txt")
            str/split-lines
            (mapv #(mapv parse-long (str/split % #"\s+")))
            (apply map vector))]
  [(->> data
        (map sort)
        (apply map (fn [x y] (abs (- x y))))
        (reduce +))
   (->> data
        ((fn [[xs ys]]
           (transduce (map #(* % (count (filter #{%} ys)))) + xs))))])
     ; [1722302 20373490]

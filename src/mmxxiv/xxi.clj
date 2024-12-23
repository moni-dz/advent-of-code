(ns mmxxiv.xxi
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(def numbers (zipmap "7894561230A" [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2] [3 1] [3 2]]))
(def directions (zipmap "^A<v>" [[0 1] [0 2] [1 0] [1 1] [1 2]]))

(defn invalid-path? [start moves spot]
  (let [d {\^ [-1 0], \< [0 -1], \v [1 0], \> [0 1]}]
    (some #{spot} (reductions #(mapv + %1 (d %2)) start (drop-last moves)))))

(def movements
  (memoize
   (fn [[from to] spot]
     (let [[dy dx] (map - to from)
           moves (apply str (mapcat #(repeat (abs %1) (if (pos? %1) %2 %3)) [dx dy] [\> \v] [\< \^]))
           perms (set (combo/permutations moves))
           removed (set (filter #(invalid-path? from % spot) perms))]
       (set (map #(str (apply str %) "A") (set/difference perms removed)))))))

(def min-seq
  (memoize
   (fn [code robot first?]
     (let [pad (if first? numbers directions)
           spot (if first? [3 0] [0 0])
           positions (map pad (cons \A code))]
       (letfn [(dist [point]
                 (let [moves (movements point spot)]
                   (if (pos? robot)
                     (apply min (map #(min-seq % (dec robot) false) moves))
                     (count (first moves)))))]
         (transduce (map dist) + 0 (partition 2 1 positions)))))))

(defn complexity [input robots]
  (time (transduce (map #(* (parse-long (str/join (drop-last %))) (min-seq % robots true))) + 0 (str/split-lines input))))

(map #(complexity (slurp "inputs/2024/21.txt") %) [2 25])

(require '[clojure.string :as str]
         '[clojure.math.combinatorics :as combo]
         '[clojure.set :as set])

(def numbers {\7 [0 0], \8 [0 1], \9 [0 2], \4 [1 0], \5 [1 1], \6 [1 2], \1 [2 0], \2 [2 1], \3 [2 2], \0 [3 1], \A [3 2]})
(def directions {\^ [0 1], \A [0 2], \< [1 0], \v [1 1], \> [1 2]})

(defn valid-path? [pos moves spot]
  (let [d {\^ [-1 0], \< [0 -1], \v [1 0], \> [0 1]}]
    (some #(= spot %) (reductions #(mapv + %1 (d %2)) pos (butlast moves)))))

(def movements
  (memoize
   (fn [[from to] spot]
     (let [[dy dx] (map - to from)
           [y x] (map #(apply str (repeat (abs %1) (if (pos? %1) %2 %3))) [dy dx] [\v \>] [\^ \<])
           perms (set (combo/permutations (str x y)))
           removed (reduce #(if (valid-path? from %2 spot) (conj %1 %2) %1) #{} perms)]
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
  (time (transduce (map #(* (parse-long (str/join (butlast %))) (min-seq % robots true))) + 0 (str/split-lines input))))

(->> [2 25] (map (partial complexity (slurp "inputs/2024/21.txt"))))

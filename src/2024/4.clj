(require '[clojure.string :as str])

(defn char-at [grid x y]
  (when (and (>= x 0) (>= y 0)
             (< y (count grid))
             (< x (count (first grid))))
    (get-in grid [y x])))

(def directions [[0 1] [1 0] [1 1] [1 -1] [0 -1] [-1 0] [-1 -1] [-1 1]])

(defn matches-word? [grid x y [dx dy]]
  (when (= (char-at grid x y) \X)
    (every? #(= (nth "XMAS" %)
                (or (char-at grid (+ x (* % dx)) (+ y (* % dy))) \.))
            (range 1 4))))

(defn count-xmas [grid]
  (let [grid (mapv vec grid)]
    (->> (for [x (range (count (first grid)))
               y (range (count grid))
               d directions]
           (matches-word? grid x y d))
         (filter identity)
         count)))

(def mas-patterns #{[\M \A \S] [\S \A \M]})

(defn x-pattern? [grid row col]
  (when (and (>= row 1) (< row (dec (count grid)))
             (>= col 1) (< col (dec (count (first grid))))
             (= (char-at grid row col) \A))
    (and (mas-patterns [(or (char-at grid (dec row) (dec col)) \.)
                        \A
                        (or (char-at grid (inc row) (inc col)) \.)])
         (mas-patterns [(or (char-at grid (dec row) (inc col)) \.)
                        \A
                        (or (char-at grid (inc row) (dec col)) \.)]))))

(defn count-x-mas [grid]
  (->> (for [x (range (count (first grid)))
             y (range (count grid))]
         (x-pattern? grid y x))
       (filter identity)
       count))

(let [data (str/split-lines (slurp "inputs/2024/4.txt"))]
  [(count-xmas data) (count-x-mas data)])

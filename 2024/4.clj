(require '[clojure.string :as str])

(defn in-bounds? [grid x y]
  (and (>= x 0)
       (>= y 0)
       (< y (count grid))
       (< x (count (first grid)))))

(defn char-at [grid x y & {:keys [default] :or {default \.}}]
  (if (in-bounds? grid x y)
    (get-in grid [y x])
    default))

(def directions
  [[0 1] [1 0] [1 1] [1 -1] [0 -1] [-1 0] [-1 -1] [-1 1]])

(defn matches-word? [grid x y [dx dy]]
  (when (= (char-at grid x y) \X)
    (let [word "XMAS"]
      (every? (fn [i]
                (= (nth word i)
                   (char-at grid
                            (+ x (* i dx))
                            (+ y (* i dy)))))
              (range 1 (count word))))))

(defn count-xmas [grid]
  (let [grid (mapv vec grid)
        height (count grid)
        width (count (first grid))]
    (->> (for [x (range width)
               y (range height)]
           [x y])
         (mapcat (fn [pos] (map #(conj pos %) directions)))
         (filter (fn [[x y dir]] (matches-word? grid x y dir)))
         count)))

(def mas-patterns #{[\M \A \S] [\S \A \M]})

(defn valid-diagonal? [grid row col [dx dy]]
  (mas-patterns [(char-at grid (- row dy) (- col dx))
                 \A
                 (char-at grid (+ row dy) (+ col dx))]))

(defn x-pattern? [grid row col]
  (let [height (count grid)
        width (count (first grid))]
    (when (and (>= row 1) (< row (dec height))
               (>= col 1) (< col (dec width))
               (= (char-at grid row col) \A))
      (and (valid-diagonal? grid row col [1 1])
           (valid-diagonal? grid row col [1 -1])))))

(defn count-x-mas [grid]
  (let [height (count grid)
        width (count (first grid))]
    (->> (for [x (range width)
               y (range height)]
           [x y])
         (filter (fn [[col row]] (x-pattern? grid row col)))
         count)))

(let [data (->> (slurp "4.txt")
                str/split-lines)]
  [(count-xmas data) (count-x-mas data)])

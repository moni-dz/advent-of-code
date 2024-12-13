(require '[clojure.string :as str]
         '[clojure.math :as math])

(defn parse-input [input offset]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map (fn [line]
              (if-let [[_ _ x y] (re-matches #"Button ([AB]): X\+(-?\d+), Y\+(-?\d+)" line)]
                [(parse-long x) (parse-long y)]
                (when-let [[_ x y] (re-matches #"Prize: X=(\d+), Y=(\d+)" line)]
                  [(+ (parse-long x) offset) (+ (parse-long y) offset)]))))
       (partition 3)
       (map (fn [[v1 v2 p]] {:v1 v1 :v2 v2 :p p}))))

(defn find-solution [{[x1 y1] :v1 [x2 y2] :v2 [px py] :p}]
  (let [t1 (/ (- (* px y2) (* x2 py)) (- (* x1 y2) (* x2 y1)))
        t2 (/ (- (* x1 py) (* px y1)) (- (* x1 y2) (* x2 y1)))]
    (when (every? #(and (>= % 0) (== % (math/floor %))) [t1 t2])
      (+ (* (long t1) 3) (long t2)))))

(let [data (slurp "inputs/2024/13.txt")]
  (->> [0 10000000000000]
       (map #(parse-input data %))
       (map #(->> % (keep find-solution) (reduce +)))
       vec))

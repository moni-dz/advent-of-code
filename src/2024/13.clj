(require '[clojure.string :as str]
         '[clojure.math :as math])

(defn parse-input [input offset]
  (->> (str/split-lines input)
       (remove str/blank?)
       (pmap (fn [line]
               (if-let [[_ _ x y] (re-matches #"Button ([AB]): X\+(\d+), Y\+(\d+)" line)]
                 [(parse-double x) (parse-double y)]
                 (when-let [[_ x y] (re-matches #"Prize: X=(\d+), Y=(\d+)" line)]
                   [(+ (parse-double x) offset) (+ (parse-double y) offset)]))))
       (partition 3)
       (pmap (fn [[a b p]] {:a a :b b :p p}))))

(defn solve [{[x1 y1] :a [x2 y2] :b [px py] :p}]
  (let [t1 (/ (- (* px y2) (* x2 py)) (- (* x1 y2) (* x2 y1)))
        t2 (/ (- (* x1 py) (* px y1)) (- (* x1 y2) (* x2 y1)))]
    (when (every? #(= % (math/floor %)) [t1 t2])
      (+ (* t1 3.0) t2))))

(->> [0 10000000000000]
     (pmap #(parse-input (slurp "inputs/2024/13.txt") %))
     (pmap #(->> % (keep solve) (reduce +)))
     (mapv long))

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
       (pmap (fn [[v1 v2 p]] {:v1 v1 :v2 v2 :p p}))))

(defn find-solution [{[x1 y1] :v1 [x2 y2] :v2 [px py] :p}]
  (let [t1 (/ (- (* px y2) (* x2 py)) (- (* x1 y2) (* x2 y1)))
        t2 (/ (- (* x1 py) (* px y1)) (- (* x1 y2) (* x2 y1)))]
    (when (every? #(= % (math/floor %)) [t1 t2])
      (+ (* t1 3.0) t2))))

(let [data (slurp "inputs/2024/13.txt")]
  (->> [0 10000000000000]
       (pmap #(parse-input data %))
       (pmap #(->> % (keep find-solution) (reduce +)))
       (mapv long)))

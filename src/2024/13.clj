(require '[clojure.string :as str])

(defn parse [input offset]
  (->> (remove str/blank? (str/split-lines input))
       (pmap #(let [[x y] (pmap parse-double (re-seq #"\d+" %))]
                (if (str/starts-with? % "Prize")
                  [(+ x offset) (+ y offset)]
                  [x y])))
       (partition 3)
       (pmap #(zipmap [:a :b :p] %1))))

(defn solve [{[ax ay] :a [bx by] :b [px py] :p}]
  (let [x (/ (- (* px by) (* bx py)) (- (* ax by) (* bx ay)))
        y (/ (- (* ax py) (* px ay)) (- (* ax by) (* bx ay)))]
    (when (every? #(zero? (mod % 1)) [x y])
      (long (+ (* x 3) y)))))

(->> [0 10000000000000]
     (pmap #(->> % (parse (slurp "inputs/2024/13.txt")) (keep solve) (reduce +))))

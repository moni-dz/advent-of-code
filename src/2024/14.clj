(defn parse-input [s]
  (->> (re-seq #"-?\d+" s)
       (map parse-long)
       (partition 4)
       (map (fn [[x y vx vy]] {:pos [x y] :vel [vx vy]}))))

(def quadrant
  (fn [[x y] [w h]]
    (when-not (or (= x (quot w 2))
                  (= y (quot h 2)))
      (inc (+ (* 2 (if (< y (quot h 2)) 0 1))
              (if (< x (quot w 2)) 0 1))))))

(defn pos-at [robots dims t]
  (set (for [{[x y] :pos [vx vy] :vel} robots]
         [(mod (+ x (* t vx)) (dims 0))
          (mod (+ y (* t vy)) (dims 1))])))

(defn step [dims {:keys [pos vel]}]
  {:pos [(mod (+ (pos 0) (vel 0)) (dims 0))
         (mod (+ (pos 1) (vel 1)) (dims 1))]
   :vel vel})

(let [robots (parse-input (slurp "inputs/2024/14.txt"))
      dims [101 103]]
  [(->> (nth (iterate #(map (partial step dims) %) robots) 100)
        (pmap :pos)
        (keep #(quadrant % dims))
        frequencies
        vals
        (apply *))
   (some #(when (= (count (pos-at robots dims %)) (count robots)) %)
         (rest (range)))])

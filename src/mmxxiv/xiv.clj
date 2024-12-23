(ns mmxxiv.xiv)

(defn parse-input [s]
  (->> (re-seq #"-?\d+" s)
       (map parse-long)
       (partition 4)
       (map #(zipmap [:p :v] (partition 2 %)))))

(def quadrant
  (fn [[px py] {:keys [w h]}]
    (when-not (or (= px (quot w 2)) (= py (quot h 2)))
      (inc (+ (* 2 (if (< py (quot h 2)) 0 1))
              (if (< px (quot w 2)) 0 1))))))

(defn p-at [robots {:keys [w h]} t]
  (set (for [{[px py] :p [vx vy] :v} robots]
         [(mod (+ px (* t vx)) w) (mod (+ py (* t vy)) h)])))

(defn step [{:keys [w h]} {[px py] :p [vx vy] :v :as robot}]
  (assoc robot :p [(mod (+ px vx) w) (mod (+ py vy) h)]))

(let [robots (parse-input (slurp "inputs/2024/14.txt"))
      dims {:w 101 :h 103}]
  [(->> (nth (iterate #(map (partial step dims) %) robots) 100)
        (pmap :p)
        (keep #(quadrant % dims))
        frequencies
        vals
        (apply *))
   (some #(when (= (count (p-at robots dims %)) (count robots)) %)
         (rest (range)))])

(require '[clojure.string :as str])

(defn parse-input [input]
  (let [[patterns designs] (str/split input #"\n\n")]
    [(str/split (str/trim patterns) #",\s*") (str/split-lines designs)]))

(defn valid-pattern? [pattern design]
  (and (<= (count pattern) (count design)) (= pattern (subs design 0 (count pattern)))))

(def valid-design?
  (memoize (fn [patterns design]
             (if (empty? design)
               true
               (->> patterns
                    (some #(and (valid-pattern? % design)
                                (valid-design? patterns (subs design (count %))))))))))

(def count-designs
  (memoize
   (fn [patterns design]
     (if (empty? design)
       1
       (->> patterns
            (keep #(when (valid-pattern? % design)
                     (count-designs patterns (subs design (count %)))))
            (reduce +))))))

(let [[patterns designs] (->> "inputs/2024/19.txt" slurp parse-input)
      possible (time (->> designs (filter #(valid-design? patterns %)) count))
      permutations (time (->> designs (map #(count-designs patterns %)) (reduce +)))]
  [possible permutations])

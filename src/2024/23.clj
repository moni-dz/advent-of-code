(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-graph [input]
  (->> (map #(str/split % #"-") (str/split-lines input))
       (reduce (fn [g [a b]] (merge-with into g {a #{b} b #{a}})) {})))

(defn triplets [g]
  (for [[a bs] g, b bs, c (g b) :when (and (pos? (compare b a)) (pos? (compare c b)) (contains? (g a) c))]
    #{a b c}))

(defn bron-kerbosch
  ([g] (bron-kerbosch g #{} (set (keys g)) #{}))
  ([g r p x]
   (if (and (empty? p) (empty? x))
     [r]
     (let [v (set/difference p (g (first (set/union p x))))]
       (->> v
            (mapcat #(let [a (g %)] (bron-kerbosch g (conj r %) (set/intersection p a) (set/intersection x a))))
            (#(if (seq v) % (bron-kerbosch g r (disj p (first v)) (conj x (first v))))))))))

(let [graph (-> "inputs/2024/23.txt" slurp parse-graph)]
  [(-> graph triplets (->> (filter #(some (fn [s] (str/starts-with? s "t")) %)) count))
   (time (-> graph bron-kerbosch (->> (apply max-key count) sort (str/join ","))))])

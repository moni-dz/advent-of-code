(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-graph [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (reduce (fn [g [a b]] (merge-with into g {a #{b} b #{a}})) {})))

(defn triplets [g]
  (for [[a bs] g, b bs, c (g b) :when (and (pos? (compare b a)) (pos? (compare c b)) (contains? (g a) c))]
    #{a b c}))

(defn bron-kerbosch
  ([g] (bron-kerbosch g #{} (set (keys g)) #{}))
  ([g r p x]
   (if (and (empty? p) (empty? x))
     [r]
     (let [nodes (first (or (seq p) (seq x)))]
       (->> (if nodes (set/difference p (g nodes)) p)
            (mapcat #(bron-kerbosch g (conj r %) (set/intersection p (g %)) (set/intersection x (g %))))
            vec)))))

(let [graph (-> "inputs/2024/23.txt" slurp parse-graph)]
  [(time (-> graph triplets (->> (filter #(some (fn [s] (str/starts-with? s "t")) %)) count)))
   (time (-> graph bron-kerbosch (->> (apply max-key count) sort (str/join ","))))])

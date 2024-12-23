(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-graph [input]
  (transduce (comp (map #(str/split % #"-")) (map (fn [[a b]] {a #{b} b #{a}})))
             (partial merge-with into) {} (str/split-lines input)))

(defn triplets [g]
  (for [[a bs] g, b bs, c (g b) :when (and (pos? (compare b a)) (pos? (compare c b)) (contains? (g a) c))]
    #{a b c}))

(defn bron-kerbosch
  ([g] (bron-kerbosch g #{} (set (keys g)) #{}))
  ([g r p x]
   (if (and (empty? p) (empty? x))
     [r]
     (let [u (first (set/union p x)) v (set/difference p (g u))]
       (if (seq v)
         (persistent!
          (reduce
           #(reduce conj! %1 (bron-kerbosch g (conj r %2) (set/intersection p (g %2)) (set/intersection x (g %2))))
           (transient []) v))
         (bron-kerbosch g r (disj p u) (conj x u)))))))

(let [graph (-> "inputs/2024/23.txt" slurp parse-graph)]
  [(time (-> graph triplets (->> (filter #(some (fn [s] (str/starts-with? s "t")) %)) count)))
   (time (-> graph bron-kerbosch (->> (apply max-key count) sort (str/join ","))))])

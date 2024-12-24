(ns mmxxiv.xxiv
  (:require [clojure.string :as str]))

(defn parse-values [lines]
  (->> lines
       (filter #(re-matches #"[xy]\d+: [01]" %))
       (map #(let [[_ wire value] (re-matches #"([xy]\d+): ([01])" %)]
               [wire (parse-long (str value))]))
       (into {})))

(defn parse-gate [line]
  (let [[_ a op b out] (re-matches #"(\w+) (AND|OR|XOR) (\w+) -> (\w+)" line)]
    {:op op :a a :b b :out out}))

(defn parse-gates [lines]
  (->> lines (filter #(re-matches #"\w+ (AND|OR|XOR) \w+ -> \w+" %)) (map parse-gate)))

(defn gate! [{:keys [op a b]} values]
  (when (and (contains? values a) (contains? values b))
    (case op
      "AND" (bit-and (get values a) (get values b))
      "OR"  (bit-or (get values a) (get values b))
      "XOR" (bit-xor (get values a) (get values b)))))

(defn simulate [[gates values]]
  (letfn [(step [g v] (reduce #(or (some->> (gate! %2 %1) (assoc %1 (:out %2))) %1) v g))]
    (->> (iterate #(step gates %) values)
         (partition 2 1)
         (drop-while (fn [[a b]] (not= a b)))
         ffirst)))

(defn input? [gate]
  (or (str/starts-with? (:a gate) "x")
      (str/starts-with? (:b gate) "x")))

(defn z-output? [gate]
  (str/starts-with? (:out gate) "z"))

(defn op? [op]
  (fn [gate] (= (:op gate) op)))

(defn find-swaps [gates]
  (let [bit-count 45
        inputs (filter #(and (input? %) ((op? "XOR") %)) gates)
        outputs (filter #(and (not (input? %)) ((op? "XOR") %)) gates)
        z-outputs (filter z-output? gates)

        bad-flags
        (set
         (concat
          (->> inputs
               (filter #(let [first? (or (= (:a %) "x00") (= (:b %) "x00"))
                              output (:out %)]
                          (and (not= output "z00")
                               (or (and first? (not= output "z00"))
                                   (and (not first?) (= output "z00"))
                                   (z-output? %)))))
               (map :out))

          (->> outputs
               (filter #(and (not= (:out %) "z00") (not (z-output? %))))
               (map :out))

          (->> z-outputs
               (filter #(let [carry? (= (:out %) (format "z%02d" bit-count))]
                          (and (not= (:out %) "z00")
                               (or (and carry? (not= (:op %) "OR"))
                                   (and (not carry?) (not= (:op %) "XOR"))))))
               (map :out))))

        bad-flags
        (concat
         bad-flags
         (->> inputs
              (filter #(let [output (:out %)]
                         (and (not (bad-flags output))
                              (not= output "z00")
                              (empty? (filter (fn [g] (or (= (:a g) output) (= (:b g) output))) outputs)))))
              (mapcat
               (fn [gate]
                 (let [intended-z (str "z" (subs (:a gate) 1))
                       matches (filter #(= (:out %) intended-z) outputs)]
                   (when (= (count matches) 1)
                     (let [match (first matches)
                           to-check [(:a match) (:b match)]
                           or-matches (filter #(and ((op? "OR") %) (some #{(:out %)} to-check)) gates)]
                       (when (= (count or-matches) 1)
                         (let [or-match-output (:out (first or-matches))
                               correct-output (first (remove #{or-match-output} to-check))]
                           (when (and (not= (:out gate) "z00")
                                      (not= correct-output "z00"))
                             [(:out gate) correct-output]))))))))))]

    (->> bad-flags sort (str/join ","))))

(let [input (-> "inputs/2024/24.txt" slurp str/split-lines)]
  [(time (->> input
              ((juxt parse-gates parse-values))
              simulate
              (filter #(str/starts-with? (key %) "z"))
              (sort-by key)
              (map val)
              reverse
              (reduce #(+ (* 2 %1) %2) 0)))
   (time (->> input parse-gates find-swaps))])

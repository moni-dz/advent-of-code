(defn process-muls [input]
  (->> input
       (re-seq #"mul\(([-+]?\d+),([-+]?\d+)\)")
       (map (fn [[_ x y]] (* (parse-long x) (parse-long y))))
       (reduce +)))

(defn process-instructions [input]
  (let [tokens (re-seq #"do\(\)|don't\(\)|mul\([0-9]+,[0-9]+\)" input)]
    (loop [tokens tokens
           total 0
           enabled? true]
      (if-let [token (first tokens)]
        (case token
          "do()" (recur (rest tokens) total true)
          "don't()" (recur (rest tokens) total false)
          (recur (rest tokens)
                 (if enabled?
                   (+ total (reduce * (map parse-long (re-seq #"\d+" token))))
                   total)
                 enabled?))
        total))))

(let [data (slurp "inputs/2024/3.txt")]
  [(process-muls data) (process-instructions data)])



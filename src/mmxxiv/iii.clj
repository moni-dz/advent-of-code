(ns mmxxiv.iii)

(defn process-instructions [input]
  (->> (re-seq #"do\(\)|don't\(\)|mul\([0-9]+,[0-9]+\)" input)
       (reduce (fn [{:keys [total enabled?]} token]
                 (case token
                   "do()" {:total total :enabled? true}
                   "don't()" {:total total :enabled? false}
                   {:total (if enabled? (+ total (reduce * (map parse-long (re-seq #"\d+" token)))) total)
                    :enabled? enabled?}))
               {:total 0 :enabled? true})
       :total))

(let [input (slurp "inputs/2024/3.txt")]
  [(transduce (comp (map #(* (parse-long (nth % 1)) (parse-long (nth % 2))))) + 0
              (re-seq #"mul\(\s*(\d+)\s*,\s*(\d+)\s*\)" input))
   (process-instructions input)])



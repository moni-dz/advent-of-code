(require '[clojure.string :as str])

(defn parse-input [input]
  (let [[a b c p] (str/split input #"\n+")
        [a b c] (map (comp parse-long second)
                     (re-seq #": (\d+)" (str/join "\n" [a b c])))
        program (mapv parse-long (re-seq #"\d+" p))]
    {:registers {:a a :b b :c c} :program program}))

(defn combo->value [operand registers]
  (get [0 1 2 3 (:a registers) (:b registers) (:c registers)] operand))

(defn shift-right [value bits]
  (quot value (bit-shift-left 1 bits)))

(defn execute-instruction [{:keys [registers program]} pc outputs]
  (let [{:keys [a b c]} registers
        opcode (nth program pc)
        operand (nth program (inc pc))
        pc (+ pc 2)
        op-value (combo->value operand registers)]
    (case opcode
      ;; adv
      0 [(assoc registers :a (shift-right a op-value)) pc outputs]

      ;; bxl, bxc
      1 [(assoc registers :b (bit-xor b operand)) pc outputs]
      4 [(assoc registers :b (bit-xor b c)) pc outputs]

      ;; bst
      2 [(assoc registers :b (mod op-value 8)) pc outputs]

      ;; jnz
      3 [registers (if (zero? a) pc operand) outputs]

      ;; out
      5 [registers pc (conj outputs (mod op-value 8))]

      ;; cdv
      6 [(assoc registers :b (shift-right a op-value)) pc outputs]
      7 [(assoc registers :c (shift-right a op-value)) pc outputs])))

(defn run-vm [{:keys [registers program]} until-output?]
  (loop [registers (or registers {:a 0 :b 0 :c 0})
         pc 0
         outputs []]
    (if (>= pc (count program))
      (str/join "," outputs)
      (let [opcode (nth program pc)]
        (if (and until-output? (= opcode 5))
          (mod (combo->value (nth program (inc pc)) registers) 8)
          (let [[new-registers new-pc new-outputs]
                (execute-instruction {:registers registers :program program} pc outputs)]
            (recur new-registers new-pc new-outputs)))))))

(defn solve [{:keys [_ program] :as input}]
  (letfn [(test-digit [d position result]
            (let [a (+ (* result 8) d)]
              (when (= (run-vm (assoc-in input [:registers :a] a) true)
                       (nth program position))
                (try-digit (dec position) a))))
          (try-digit [position result]
            (if (neg? position)
              result
              (some #(test-digit % position result) (range 8))))]
    (try-digit (dec (count program)) 0)))

(defn test-value [a {:keys [_ program]}]
  (let [output (-> (run-vm {:registers {:a a :b 0 :c 0}
                            :program program}
                           false)
                   (str/split #",")
                   (->> (mapv parse-long)))]
    (println "output:" output)
    (println "target:" program)
    (= output (vec program))))

(let [prgm (->> "inputs/2024/17.txt" slurp parse-input)
      _sample {:registers {:a 729 :b 0 :c 0} :program [0,1,5,4,3,0]}
      solution (solve prgm)
      _ (println (test-value solution prgm))]
  [(run-vm prgm false) solution])

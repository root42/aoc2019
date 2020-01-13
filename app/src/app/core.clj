(ns app.core)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (clojure.string/split-lines input))) 
  )

(defn read-input-csv
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (re-seq #"[^,\n]+" input))) 
  )

(defn fuel-req
  [mass]
  (int (- (Math/floor (/ mass 3) ) 2) )
  )

(defn fuel-reqs
  [modules]
  (map #(fuel-req %) modules )
  )

(defn rec-fuel-req
  [mass]
  (if (<= mass 0)
    0
    (+ mass (rec-fuel-req (fuel-req mass))))
  )

(defn rec-fuel-reqs
  [modules]
  (reduce + (map #(rec-fuel-req (fuel-req %)) modules))
  )

(defn evaluate-opcode
  "Evaluates the opcode and mutates the vector"
  [program, instruction]
  (let [opcode (nth program instruction)
        opindex1 (nth program (+ instruction 1))
        opindex2 (nth program (+ instruction 2))
        destination (nth program (+ instruction 3))
        operand1 (nth program opindex1)
        operand2 (nth program opindex2)
        ]
    (case opcode
      1 (assoc program destination (+ operand1 operand2))
      2 (assoc program destination (* operand1 operand2))
        )
    )
  )

(defn run-int-program
  "Runs the given integer program"
  [program, instruction]
  (loop [program program
         instruction instruction]
      (if (= 99 (nth program instruction))
        (nth program 0)
        (recur (evaluate-opcode program instruction) (+ instruction 4))))
  )

(defn run-gravity-assist
  [input]
  (->
   (into [] input)
   (assoc 1 12)
   (assoc 2 2)
   (run-int-program 0)
   )
  )

(defn find-noun-verb
  [input]
  (first
   (for [noun (range 100)
         verb (range 100)
         :let [result  (->
                        (into [] input)
                        (assoc 1 noun)
                        (assoc 2 verb)
                        (run-int-program 0)
                        )]
         :when (= 19690720 result)
         ]
     (+ verb (* 100 noun))
     )))

(defn -main
  "Advent of Code 2019."
  [& args]
  (let [input (read-input "1.txt")]
    (println "1.1 Sum of fuel requirements: " (reduce + (fuel-reqs input)))
    (println "1.2 Sum of fuel requirements: " (rec-fuel-reqs input))
    )
  (let [input (read-input-csv "2.txt")]
    (println "2.1 1202 Program Alarm - gravity assist: "
             (run-gravity-assist input))
    )
  (let [input (read-input-csv "2.txt")]
    (println "2.2 1202 Program Alarm - noun verb: "
             (find-noun-verb input))
    )
  )

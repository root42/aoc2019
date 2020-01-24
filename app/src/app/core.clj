(ns app.core)
(require '(clojure set))

;; Reader functions for the different inputs

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

(defn read-wire-definitions
  [input-file]
  (let [input (slurp input-file)]
    (map #(clojure.string/split % #",") (clojure.string/split-lines input)
         )
    )
  )

;; Day 4

(defn has-two-same-digits
  [number]
  (< (count (set number)) (count number))
  )

(defn is-monotonic
  [number]
  (= number (sort number))
  )

(defn compute-all-passwords
  []
  (->> (range 240920 (inc 789857))
       (map str)
       (map seq)
       (filter is-monotonic)
       (filter has-two-same-digits)
       (count)
       )
  )

(defn has-an-exact-double
  [number]
  (->> number
       (frequencies)
       (vals)
       (some #{2})
       )
  )

(defn compute-all-passwords-with-double
  []
  (->> (range 240920 (inc 789857))
       (map str)
       (map seq)
       (filter is-monotonic)
       (filter has-an-exact-double)
       (count)
       )
  )

;; Day 3

(defn point-in-direction
  [[x y] direction distance]
  (case direction
    \R [(+ x distance) y]
    \L [(- x distance) y]
    \U [x (+ y distance)]
    \D [x (- y distance)]
    )
  )

(defn generate-points
  [start definition]
  (let [direction (first definition)
        length (Integer. (clojure.string/join (rest definition)))
        ]
    ; return the end position of the wire and the generated points, WITHOUT the
    ; first point (hence the inc). or else you get an intersection at [0 0],
    ; for example. Also range goes to n-1, so the inc is necessary there as
    ; well.
    [
     (point-in-direction start direction length)
     (map #(point-in-direction start direction (inc %)) (range length))
     ]
    )
  )

(defn make-wire
  [wire-definition]
  (loop [pos [0 0]
         points []
         definitions wire-definition
         ]
    (if (empty? definitions)
      points
      (let [definition (first definitions)
            [new-pos new-points] (generate-points pos definition)]
        (recur new-pos (concat points new-points) (rest definitions))
        )
      )
    )
  )

(defn manhattan
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn intersect-wires
  [wires]
  (->> wires
       (map set)
       (apply clojure.set/intersection)
       )
  )

(defn compute-manhattan-to-intersection
  [input]
  (->> input
       (map make-wire)
       (intersect-wires)
       (map #(manhattan % [0 0]))
       (reduce min)
       )
  )

(defn compute-minimal-combined-steps
  [input]
  (let [wires (map make-wire input)
        wire1 (first wires)
        wire2 (first (rest wires))
        ]
    ; We can find the minimal path by looking up each intersection in the
    ; generated wire. The method .indexOf will give us the first index in the
    ; vector, which is the minimal distance. We have to add one extra step for
    ; each wire, because the origin is missing from this computation.
    (->> wires
         (intersect-wires)
         (map #(+ 2 (.indexOf wire1 %) (.indexOf wire2 %)))
         (reduce min)
         )
    )
  )

;; Day 2

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

;; Day 1

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

;; Main program

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
    (println "2.2 1202 Program Alarm - noun verb: "
             (find-noun-verb input))
    )
  (let [input (read-wire-definitions "3.txt")]
    (println "3.1 Manhattan distance: "
             (compute-manhattan-to-intersection input)
             )
    (println "3.2 Minimize signal delay: "
             (compute-minimal-combined-steps input)
             )
    )
  (println "4.1 Compute number of valid passwords"
           (compute-all-passwords)
           )
  (println "4.2 Compute number of valid passwords with at least one double"
           (compute-all-passwords-with-double)
           )
  )

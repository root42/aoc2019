(ns app.core)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (clojure.string/split-lines input))) 
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

(defn -main
  "Advent of Code 2019."
  [& args]
  (let [input (read-input "1.txt")]
    (println "1.1 Sum of fuel requirements: " (reduce + (fuel-reqs input)))
    (println "1.2 Sum of fuel requirements: " (rec-fuel-reqs input))
    )
  )

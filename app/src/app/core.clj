(ns app.core)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (clojure.string/split-lines input))) 
  )

(defn fuel-reqs
  [modules]
  (map #(int (- (Math/floor (/ % 3) ) 2))  modules)
  )

(defn -main
  "Advent of Code 2019."
  [& args]
  (let [input (read-input "1.txt")]
    (println "Sum of fuel requirements: " (reduce + (fuel-reqs input)))
    )
  )

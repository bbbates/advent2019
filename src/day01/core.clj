(ns day01.core)

(defn- fuel-required
  [mass]
  (-> mass
      (/ 3)
      (int)
      (- 2)))

(assert (= (fuel-required 12) 2))
(assert (= (fuel-required 14) 2))
(assert (= (fuel-required 1969) 654))
(assert (= (fuel-required 100756) 33583))

(defn solve-part-one [data]
  (->>
    data
    (map fuel-required)
    (reduce +)))

(defn- fuel-masses-for-module
  [mass]
  (rest (take-while pos? (iterate fuel-required mass))))

(assert (= (fuel-masses-for-module 12) [2]))
(assert (= (fuel-masses-for-module 14) [2]))
(assert (= (fuel-masses-for-module 1969) [654 216 70 21 5]))
(assert (= (fuel-masses-for-module 100756) [33583 11192 3728 1240 411 135 43 12 2]))

(defn solve-part-two [data]
  (->> data (mapcat fuel-masses-for-module) (reduce +)))

(defn -main []
  (let [data (map #(Integer/parseInt %) (line-seq (clojure.java.io/reader "src/day01/input.txt")))]
    (println "Part 1 >>> " (solve-part-one data))
    (println "Part 2 >>> " (solve-part-two data))))


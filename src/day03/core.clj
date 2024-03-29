(ns day03.core)

(def wire-dirs {"R" :right "U" :up "L" :left "D" :down})

(defn- path-for-instr
  [[start-x start-y] [dir m]]
  (let [[end-x end-y trim-range-fn trim-path-fn]
        (case dir
          :up [start-x (+ start-y (inc m)) identity rest]
          :down [start-x (- start-y m) reverse identity]
          :left [(- start-x m) start-y reverse identity]
          :right [(+ start-x (inc m)) start-y identity rest])
        xs (trim-range-fn (apply range (sort [start-x end-x])))
        ys (trim-range-fn (apply range (sort [start-y end-y])))]
    (trim-path-fn
      (map vector
           (or (seq xs) (repeat start-x))
           (or (seq ys) (repeat start-y))))))

(defn- path-for-wire
  [wire-instrs]
  (vec (reduce (fn [coords instr]
                 (concat coords (path-for-instr (last coords) instr)))
               [[0 0]] wire-instrs)))

(defn- coords-for-wire [wire-instrs]
  (disj (set (path-for-wire wire-instrs)) [0 0]))

(defn- manhattan-distance [[start-x start-y] [end-x end-y]]
  (+ (Math/abs (- end-x start-x)) (Math/abs (- end-y start-y))))

(defn solve-part-one [wire-paths]
  (let [wire-coords (map coords-for-wire wire-paths)
        intersections (apply clojure.set/intersection wire-coords)]
    (manhattan-distance [0 0] (first (sort-by (partial manhattan-distance [0 0]) intersections)))))

(defn- parse-wire-instrs [dirs]
  (let [split-dirs (clojure.string/split dirs #",")]
    (map (fn [d]
           (let [[_ dir m] (re-find #"([RULD])(\d+)" d)]
             [(wire-dirs dir) (Integer/parseInt m)]))
         split-dirs)))

(assert (= (parse-wire-instrs "R1000,U200,D20,L101")
           [[:right 1000] [:up 200] [:down 20] [:left 101]]))


(assert (= (solve-part-one
             (->> (clojure.string/split "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" #"\n")
                  (map parse-wire-instrs))) 159))
(assert (= (solve-part-one
             (->> (clojure.string/split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #"\n")
                  (map parse-wire-instrs))) 135))


(defn solve-part-two [wire-paths]
  (let [[wire-1 wire-2] (map path-for-wire wire-paths)
        intersections (apply clojure.set/intersection (map coords-for-wire wire-paths))]
    (->> (map (fn [intersection]
                (+ (.indexOf wire-1 intersection)
                   (.indexOf wire-2 intersection)))
              intersections)
         (sort)
         (first))))


(assert (= (solve-part-two
             (->> (clojure.string/split "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" #"\n")
                  (map parse-wire-instrs))) 610))
(assert (= (solve-part-two
             (->> (clojure.string/split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #"\n")
                  (map parse-wire-instrs))) 410))

(defn -main []
  (let [wires-lines (-> (slurp "src/day03/input.txt")
                        (clojure.string/trim)
                        (clojure.string/split #"\n"))
        wires (map parse-wire-instrs wires-lines)]
    (println "Part 1>>> " (solve-part-one wires))
    (println "Part 2>>> " (solve-part-two wires))))

(require 
  '[clojure.string :refer [split join]])

(defn cartesian [as bs]
  (for [a as b bs]
     [a b]))

(defn consecutive-pairs 
  ([n xs]
    (consecutive-pairs n (count xs) xs))
  ([n x xs]
    (let [pair #(->> (drop % xs) (take n))]
    (->> (range 0 (- x (dec n)))
         (map pair)))))

(defn transpose [grid]
  (->> (apply interleave grid)
       (partition (count grid))))

(defn flip [grid]
  (map reverse grid))

; n sized pairs from left to right
(defn right-pairs [n grid]
  (mapcat #(consecutive-pairs n %) grid))

; single n sized diag pair
(defn diag-pair [n rowSize pts]
  (let [offset (fn [colNum rowSize]
                  (-> (* colNum rowSize) (+ colNum)))
        offsets (map offset (range 0 n) (repeat rowSize))]
  (->> (map #(drop % pts) offsets)
       (apply interleave)
       (take n))))

; generate all cell positions in a (rowSize - n) square sub grid.
(defn cell-positions [n rowSize]
  (let [rowNums (range 0 (inc (- rowSize n)))
        colNums (range 0 (inc (- rowSize n)))]
  (cartesian rowNums colNums)))

; generates single n sized diag pair given a cell position
(defn relative-diag-pair [n rowSize pts [rowNum colNum]]
  (let [offset (-> (* rowNum rowSize) (+ colNum))]
  (->> (drop offset pts)
       (diag-pair n rowSize))))

; sequence of n sized diagonal pairs from left to right
; at every cell pos in a (rowSize - n) square sub grid.
(defn right-diag-pairs [n rowSize grid]
  (let [pts (flatten grid)]
  (->> (cell-positions n rowSize)
       (map #(relative-diag-pair n rowSize pts %)))))

(defn pairs [pts]
  (let [grid  (partition 20 pts)
        right (right-pairs 4 grid)
        left  (right-pairs 4 (transpose grid))
        rdiag (right-diag-pairs 4 20 grid)
        ldiag (right-diag-pairs 4 20 (flip grid))]
  (concat right left rdiag ldiag)))

(defn solve [& pts]
  (->> (pairs pts)
       (map #(apply * %))
       (reduce max)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")
        safeInt #(try (Integer/parseInt %)
                      (catch Exception e %))]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)
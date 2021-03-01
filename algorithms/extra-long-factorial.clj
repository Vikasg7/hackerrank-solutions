(require 
  '[clojure.string :refer [split join]])

(defn digits [n]
  (let [toInt #(->> (str %) (read-string))]
  (->> (str n)
       (map toInt))))

(defn cartesian [as bs]
  (for [a as 
        b bs]
      [a b]))

(defn withRevIdx [xs]
  (let [cnt (dec (count xs))
        idx (range cnt -1 -1)]
  (map vector idx xs)))

(defn mult [a b]
  (let [nCoef #(->> (digits %) (withRevIdx))
        multCoefPair (fn [[[na a] [nb b]]]
                        [(+ na nb) (* a b)])
        addCoef (fn [[k cs]]
                   (apply + (map second cs)))
        addCoefByN #(->> (group-by first %)
                         (into (sorted-map))
                         (map addCoef))
        byCarry (fn [[lc & prv] coef]
                   (let [nCoef (+ coef lc)
                         cur (mod nCoef 10)
                         nc  (if (< nCoef 10) 0 (quot nCoef 10))]
                   (conj prv cur nc)))
        zeroIfEmpty #(if (empty? %) "0" %)
        toResult #(->> (drop-while zero? %)
                       (apply str)
                       (zeroIfEmpty))]
  (->> (cartesian (nCoef a) (nCoef b)) ; list combos of of [N Coef]
       (map multCoefPair) ; list of [N Coef]
       (addCoefByN) ; list of Coef grouped(+) by N
       (reduce byCarry [0])
       (toResult))))

(defn factorial [n]
  (->> (range 2 (inc n))
       (reduce mult 1)))

(defn solve [n]
  (factorial n))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map read-string))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)
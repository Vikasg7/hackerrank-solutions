(require 
  '[clojure.string :refer [split join]])

(defn digits [n]
  (let [toInt #(->> (str %) (read-string))]
  (map toInt (str n))))

(defn append-zeros [a b]
  (let [[ca cb] (map count [a b])
        zCnt (Math/abs (- ca cb))
        addZerosTo #(concat % (repeat zCnt 0))]
  (cond (< ca cb) [(addZerosTo a) b] 
        (< cb ca) [a (addZerosTo b)]
        :else [a b])))

(defn byCarry [[lc & prv] n]
  (let [nn (+ n lc)
        cur (mod nn 10)
        nc  (quot nn 10)]
  (conj prv cur nc)))

(defn str-add [a b]
  (let [bits (comp reverse digits)
       [as bs] (->> (map bits [a b])
                    (apply append-zeros))]
  (->> (map + as bs)
       (reduce byCarry [0])
       (drop-while zero?)
       (reduce str))))

(defn solve [T & nz]
  (->> (reduce str-add nz)
       (take 10)
       (reduce str)))

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
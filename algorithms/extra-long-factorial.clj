(require 
  '[clojure.string :refer [split join]])

(defn debug [n]
  (println n)
  n)

(defn digits [n]
  (let [toInt #(->> (str %) (read-string))]
  (map toInt (str n))))

(defn zeroIfEmpty [ls]
  (if (empty? ls) (list 0) ls))

(defn byCarry [[lc & prv] n]
  (let [nn (+ n lc)
        cur (mod nn 10)
        nc  (quot nn 10)]
  (conj prv cur nc)))

(defn add-products [cnt ls]
  (cond (< cnt 2) (first ls)
        :else     (reduce (partial mapv +) ls)))

(defn mult [ls n]
  (mapv #(* % n) ls))

(defn add-zeros [t ls]
  (let [t (dec t)
        zeros #(concat (repeat % 0) %2 (repeat (- t %) 0))]
  (map-indexed zeros ls)))

(defn list-mult [a b]
  (let [[as bs]  (map reverse [a b])
        products (map #(mult as %) bs)
        pCnt     (count b)]
  (->> (add-zeros pCnt products)
       (filter #(some pos? %))
       (add-products pCnt)
       (reduce byCarry [0])
       (drop-while zero?)
       (zeroIfEmpty))))

(defn str-mult [& ls]
  (->> (map digits ls)
       (reduce list-mult)
       (reduce str)))

(defn factorial [n]
  (->> (range 2 (inc n))
       (reduce str-mult 1)))

(defn solve [n]
  (factorial n))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn safeInt [n]
  (try (Integer/parseInt n)
  (catch Exception e n)))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)
(require 
  '[clojure.string :refer [split join]])

(defn debug [n]
  (println n)
  n)

(defmacro defn-memo [n & body]
  `(def ~n (memoize (fn ~body))))

(defn digits [n]
  (let [toInt #(->> (str %) (read-string))]
  (map toInt (str n))))

(defn zeroIfEmpty [ls]
  (if (empty? ls) (list 0) ls))

(defn append-n-zeros [n ls] 
  (concat ls (repeat n 0)))

(defn prepend-n-zeros [n ls] 
  (concat (repeat n 0) ls))
  
(defn prepend-zeros [& ls]
  (let [lns   (map count ls)
        mln   (apply max lns)
        zCnts (map - (repeat mln) lns)]
  (map prepend-n-zeros zCnts ls)))

(defn byCarry [[lc & prv] n]
  (let [nn (+ n lc)
        cur (mod nn 10)
        nc  (quot nn 10)]
  (conj prv cur nc)))

(defn reduce-right 
  ([f ls]   (reduce f (reverse ls)))
  ([f s ls] (reduce f s (reverse ls))))

(defn list-add [& ls]
  (->> (apply prepend-zeros ls)
       (apply map +)
       (reduce-right byCarry [0])
       (drop-while zero?)
       (zeroIfEmpty)))

(defn mult-by [ls n]
  (->> (map #(* % n) ls)
       (reduce byCarry [0])
       (drop-while zero?)
       (zeroIfEmpty)))

(defn list-mult [a b]
  (let [[as bs] (map reverse [a b])]
  (->> (map (partial mult-by as) bs)
       (map-indexed append-n-zeros)
       (reduce list-add))))

(defn str-mult [& ls]
  (->> (map digits ls)
       (reduce list-mult)
       (reduce str)))

(defn to-base [b n]
  (Integer/toString n 2))

(defn bin-exp [n e]
  (let [be (to-base 2 e)
        pred (fn [r b]
                (case b
                   \1 (list-mult (list-mult r r) [n])
                   \0 (list-mult r r)))]
  (reduce pred [1] be)))

(defn expt [b e]
  (.pow b e))

(defn sum-tbl [nz]
  (let [pred (fn [[[ln lp] & res :as tbl] n]
                (let [np (*' lp (expt 2M (- n ln)))]
                (conj tbl [n np])))]
  (->> (dedupe (sort nz))
       (reduce pred [[0 1]])
       (into {}))))

(defn digit-sum [n]
  (apply + (digits n)))

(defn power-digit-sum [tbl n]
  (digit-sum (get tbl n)))

(defn solve [T & nz]
  (let [tbl (sum-tbl nz)]
  (map (partial power-digit-sum tbl) nz)))

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

;; (time (main))
(main)
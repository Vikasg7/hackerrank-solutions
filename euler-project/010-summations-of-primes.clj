(require 
  '[clojure.string :refer [split join]])

; summerize coll by another coll using compFn and accFn
(defn summerize-by [compFn byColl accFn coll]
  (let [iter (fn [[ret coll] k]
                (let [[used nColl] (split-with #(compFn % k) coll)
                      v (reduce accFn used)
                      nRet (conj ret [k v])]
                [nRet nColl]))]
  (->> (distinct byColl)
       (reduce iter [[] coll])
       (first))))

; running accumulation for values in a pair
(defn pair-reductions [f mp]
  (let [fsts (map first mp)
        snds (map second mp)]
  (->> (reductions f snds)
       (map vector fsts))))

(defn prime? 
  ([n]
    (let [oddNums (iterate #(+ % 2) 3)]
    (prime? n (cons 2 oddNums))))
  ([n [i & is]]
    (let [q (quot n i)
          r (mod n i)]
    (cond (< n 2)       false
          (zero? r)     false
          (> (* i i) n) true
          :else         (recur n is)))))

(defn primes
  ([]
    (primes 2))
  ([f]
    (let [from (if (odd? f) (+ f 2) (+ f 1))
          oddNums (iterate #(+ % 2) from)]
    (lazy-seq (cons f (filter prime? oddNums))))))

(defn prime-sum-map [nz]
  (let [sorted (sort nz)]
  (->> (summerize-by <= sorted + (primes))
       (pair-reductions +)
       (into {}))))

(defn solve [t & nz]
  (let [tbl (prime-sum-map nz)
        prime-sum #(get tbl % 0)]
  (map prime-sum nz)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words   #(split % #"\s")
        safeInt #(try (Integer/parseInt %)
                      (catch Exception e %))]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)
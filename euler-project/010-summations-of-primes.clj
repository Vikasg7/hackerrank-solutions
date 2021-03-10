(require 
  '[clojure.string :refer [split join]])

; summerize coll by another coll using compFn and accFn
(defn summerize-by [compFn byColl accFn coll]
  (let [iter (fn [[ret coll] k]
                (let [[used nColl] (split-with #(compFn % k) coll)
                      v (reduce accFn used)
                      nRet (if (contains? ret k) 
                              (identity ret)
                              (assoc ret k v))]
                [nRet nColl]))
        ret (sorted-map)]
  (first (reduce iter [ret coll] byColl))))

; running accumulation for values in a map
(defn map-reductions [f mp]
  (->> (reductions f (vals mp))
       (map vector (keys mp))
       (into {})))

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
       (map-reductions +))))

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
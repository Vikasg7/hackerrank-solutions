(require 
  '[clojure.string :refer [split join]])

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
  (let [iter (fn [[_ tot lp] N]
                (let [pms  (take-while #(<= % N) (primes lp))
                      nlp  (last pms)
                      ntot (reduce + tot (rest pms))]
                [N ntot nlp]))]
  (->> (sort nz)
       (filter #(>= % 2))
       (reductions iter [2 2 2]) ; [N total last-prime]
       (map (juxt first second))
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
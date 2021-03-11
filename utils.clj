(ns hackerrank.utils)

; group-by but with user-defined acc function
(defn group-with [compFn accFn sv coll]
  (let [iter (fn [ret v]
                (if-let [k (compFn v)]
                  (->> (accFn (get ret k sv) v)
                       (assoc! ret k))
                  (reduced ret)))
        ret (transient {})]
  (persistent! (reduce iter ret coll))))

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

; running accumulation for values in a map
(defn map-reductions [f mp]
  (->> (reductions f (vals mp))
       (map vector (keys mp))
       (into {})))

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

(def primes' (primes))

;; Sieve of Eratosthenes
(defn sieve 
  ([n] 
    (sieve primes' n))
  ([[i & is :as ps] n]
    (let [q (quot n i)
          r (mod n i)]
    (cond (zero? r)     (lazy-seq (cons i (sieve ps q)))
          (> (* i i) n) (when (> n 1) (lazy-seq [n]))
          :else         (recur is n)))))

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

(defn sum-of-series [n]
  (-> (* n (+ n 1)) 
      (/ 2)))

(defn sum-of-squares [n]
  (-> (* n (+ n 1) (+ (* 2 n) 1))
      (/ 6)))

(defn square [n]
  (* n n))

(defn gcd [a b]
  (cond (zero? a) b
        (zero? b) a
        :else     (recur b (mod a b))))

(defn lcm [a b]
  (-> (* a b)
      (/ (gcd a b))))

(defn even-fibs
  ([] 
    (even-fibs 2 8))
  ([a b]
    (let [nxt (-> (*' 4 b) (+' a))]
    (lazy-seq (cons a (even-fibs b nxt))))))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
    (when-let [s (seq coll)]
      (if (pred (first s))
        (lazy-seq (cons (first s) nil))
        (lazy-seq (cons (first s) (take-until pred (rest s)))))))

(defn count-factors [n]
  (let [exponents #(vals (frequencies %))]
  (if (= n 1) 1
    (->> (exponents (sieve n))
         (map inc)
         (reduce *)))))

(defn conj-once [pairs [f s]]
  (if (some #(= f (first %)) pairs) 
    (identity pairs)
    (conj pairs [f s])))
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

(defn digits [n]
  (let [toInt #(->> (str %) (read-string))]
  (map toInt (str n))))

(defn cartesian [as bs]
  (for [a as 
        b bs]
      [a b]))

(defn zeroIfEmpty [ls]
  (if (empty? ls) (list 0) ls))

(defn append-zeros [& ls]
  (let [lns (map count ls)
        mln (apply max lns)
        zCnts (map - (repeat mln) lns)
        reptz #(repeat % 0)]
  (->> (map reptz zCnts)
       (map concat ls))))

(defn byCarry [[lc & prv] n]
  (let [nn (+ n lc)
        cur (mod nn 10)
        nc  (quot nn 10)]
  (conj prv cur nc)))
       
(defn list-add [& ls]
  (->> (map reverse ls)
       (reduce append-zeros)
       (apply map +)
       (reduce byCarry [0])
       (drop-while zero?)
       (zeroIfEmpty)))

(defn str-add [& ls]
  (->> (map digits ls)
       (reduce list-add)
       (reduce str)))

(defn mult-by [n ls]
  (->> (map #(* % n) ls)
       (reduce byCarry [0])
       (drop-while zero?)
       (zeroIfEmpty)))

(defn append-n-zeros [n ls]
  (concat ls (repeat n 0)))

(defn list-mult [a b]
  (let [[as bs] (map reverse [a b])]
  (->> (cartesian bs (list as))
       (map #(apply mult-by %))
       (map-indexed append-n-zeros)
       (reduce list-add))))

(defn str-mult [& ls]
  (->> (map digits ls)
       (reduce list-mult)
       (reduce str)))
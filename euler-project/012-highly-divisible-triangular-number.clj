(require 
  '[clojure.string :refer [split join]])

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
    (when-let [s (seq coll)]
      (if (pred (first s))
        (lazy-seq (cons (first s) nil))
        (lazy-seq (cons (first s) (take-until pred (rest s)))))))

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

(defn triangle-facCnt-tbl [nz]
  (let [tri-nums (reductions + (iterate inc 1))
        max-div  (apply max nz)
        facCnts (->> (map count-factors tri-nums) 
                     (take-until #(> % max-div)))]
  (->> (map vector facCnts tri-nums)
       (reduce conj-once []))))

(defn find-facCnt [tbl n]
  (let [facCnt first
        tri-num second
        pred? #(when (> (facCnt %) n) 
                  (tri-num %))]
  (some pred? tbl)))

(defn solve [T & nz]
  (let [tbl (triangle-facCnt-tbl nz)]
  (map #(find-facCnt tbl %) nz)))

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
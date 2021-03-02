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

(def primes 
  (let [oddNums (iterate #(+ % 2) 3)]
  (lazy-seq (cons 2 (filter prime? oddNums)))))

;; Sieve of Eratosthenes
(defn sieve 
  ([n] 
    (sieve primes n))
  ([[i & is :as ps] n]
    (let [q (quot n i)
          r (mod n i)]
    (cond (zero? r)     (lazy-seq (cons i (sieve ps q)))
          (> (* i i) n) (when (> n 1) (lazy-seq [n]))
          :else         (recur is n)))))

;; primes in terms of sieve
;; (def primes 
;;   (let [oddNums (iterate #(+ % 2) 3)]
;;   (lazy-seq (cons 2 (filter #(<= % (first (factor primes %))) oddNums)))))

(defn max-prime-factor [n]
  (last (sieve n)))

(defn solve [t & nz]
  (map max-prime-factor nz))

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
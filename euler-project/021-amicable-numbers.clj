(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn debug [n]
  (println n)
  n)

(defn memoize-arr [size f]
  (let [cache (int-array size -1)]
  (fn [n]
     (let [v (if (< n size) (aget cache n) -1)]
     (cond (neg? v) (let [nv (f n)] 
                    (when (< n size) (aset-int cache n nv))
                    nv)
           :else    v)))))

(def memoije (partial memoize-arr 100000))

(defmacro defn-memo [n & body]
  `(def ~n (memoije (fn ~body))))

(defn flip 
  ([f]
    (partial flip f))
  ([f & ls]
    (apply f (reverse ls))))

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
    (cond (< n 2)       nil
          (zero? r)     (lazy-seq (cons i (sieve ps q)))
          (> (* i i) n) (when (> n 1) (lazy-seq [n]))
          :else         (recur is n)))))

;; [(X^a+1-1)/X-1]
(defn sum-factor [[f e]]
    (-> (Math/pow f (inc e))
        (- 1)
        (/ (- f 1))
        (int)))

;; Sum of factors of N = [(X^a+1-1)/X-1] x [(Y^b+1-1)/Y-1] x [(Zc^+1-1)/Z-1]
(defn-memo factor-sum [n]
  (let [fe  (->> (frequencies (sieve n)) (into []))
        c   (count fe)]
  (cond (zero? c) 0
        (= 1 c)   1
        :else     (->> (map sum-factor fe)
                       (reduce * 1)
                       (flip - n)))))
(defn amicable? [n]
  (let [a (factor-sum n)
        b (factor-sum a)]
  (and (not= a b)
       (= n b))))

(def amicables
  (->> (iterate inc 1)
       (filter amicable?)))

(defn amicable-sum [n]
  (->> (take-while #(<= % n) amicables)
       (reduce + 0)))

(defn solve [t & nz]
  (map amicable-sum nz))

(defn safeInt [n]
  (try (Long/parseLong n)
  (catch Exception e n)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (S/join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(S/split % #"\s+")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)
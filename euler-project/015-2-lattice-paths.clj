(require 
  '[clojure.string :refer [split join]])

(defn mod-mult [m a b]
  (-> (* (mod b m)
         (mod a m))
      (mod m)))

(defn mod-exp [m a b]
  (cond (zero? a) 0
        (zero? b) 1
        (odd? b)  (mod-mult m a (mod-exp m a (dec b)))
        (even? b) (let [t (mod-exp m a (/ b 2))] 
                  (mod-mult m t t))
        :else     (throw "mod-exp not yet implemented for neg numbers!")))

(defn mod-mult-inv [m a]
  (cond (= a 1) 1
        :else   (mod-exp m a (- m 2))))

;; binom-coef 
;; = ((n * (n-1) ... (n - k + 1)) / k!) mod m
;; = ((n * (n-1) ... (n - k + 1)) * mod-inv k! m) mod m
(defn binom-coef [n k]
  (let [m (-> (Math/pow 10 9) (+ 7) (int))
        inv-mod-k! (map (partial mod-mult-inv m) (range k 1 -1))]
  (->> (concat (range n (- n k) -1) inv-mod-k!)
       (reduce (partial mod-mult m) 1))))

(defn lattice-path [[n m]]
  (binom-coef (+ n m) m))

(defn solve [T & nz]
  (let [nms (partition 2 nz)]
  (map lattice-path nms)))

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
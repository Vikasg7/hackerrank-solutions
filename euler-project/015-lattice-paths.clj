(require 
  '[clojure.string :refer [split join]])

(defmacro defn-memo [n & body]
  `(def ~n (memoize (fn ~body))))

(defn modulo [n]
  (let [m (-> (Math/pow 10 9) (+ 7))]
  (int (mod n m))))

(defn-memo binom-coef [n k]
  (cond (> k n)    0
        (= n k)    1
        (zero? k)  1
        :else      (+ (modulo (binom-coef (dec n) (dec k)))
                      (modulo (binom-coef (dec n) k)))))

;; Stack safe trampolined version
;; (defn-memo binom-coef [n k]
;;   (let [inner (fn [n k]
;;                   (cond (> k n)    0
;;                         (= n k)    1
;;                         (zero? k)  1
;;                         :else      #(+ (modulo (binom-coef (dec n) (dec k)))
;;                                        (modulo (binom-coef (dec n) k)))))]
;;   (trampoline inner n k)))

(defn lattice-path [[n m]]
  (modulo (binom-coef (+ n m) m)))

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
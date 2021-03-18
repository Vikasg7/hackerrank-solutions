(require 
  '[clojure.string :refer [split join]])

(defn memoize-arr [size f]
  (let [cache (int-array size -1)]
  (fn [n]
     (let [v (if (< n size) (aget cache n) -1)]
     (cond (neg? v) (let [nv (f n)] 
                    (when (< n size) (aset-int cache n nv))
                    nv)
           :else    v)))))

(def memoije (partial memoize-arr 5000000))

(defmacro defn-memo [n & body]
  `(def ~n (memoije (fn ~body))))

(defn collatz-odd [n]
  (-> (* n 3) (+ 1) (/ 2)))

;; A little bit slower trampolined version
;; Its stack safe and passes all the tests.
;; take 1 second more than recursive version.
;; (defn-memo collatz-steps [n]
;;   (let [inner (fn [n]
;;                  (cond (= n 1)   0
;;                        (even? n) #(inc (collatz-steps (/ n 2)))
;;                        :else     #(+ 2 (collatz-steps (collatz-odd n)))))]
;;   (trampoline inner n)))

(defn-memo collatz-steps [n]
  (cond (= n 1)   0
        (even? n) (inc (collatz-steps (/ n 2)))
        :else     (+ 2 (collatz-steps (collatz-odd n)))))

(defn max-steps-starts [steps]
  (let [pred (fn [[c ms cltz] s]
                (cond (>= s ms) [(inc c) s  (conj cltz c)]
                      :else     [(inc c) ms cltz]))]
  (last (reduce pred [1 0 []] steps))))

(defn max-start [starts n]
  (last (take-while #(<= % n) starts)))

(defn steps [nz]
  (let [t (inc (apply max nz))
        rng (range 1 t)]
  (map collatz-steps rng)))

(defn solve [T & nz]
  (let [starts (max-steps-starts (steps nz))]
  (map (partial max-start starts) nz)))

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
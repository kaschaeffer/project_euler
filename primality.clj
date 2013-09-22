; Primality testing algorithms

; First need algorithm for modular exponentiation
; computes 
; (a^n) (mod p) 
; efficiently

; get binary representation of some number
(defn binary [n]
    (reverse (map #(mod % 2) (take-while #(> % 0) (iterate #(quot % 2) n)))))

(defn naive-mod-exp [mod-p a n]
    (reduce #(mod (* %1 %2) mod-p) (take n (iterate identity a))))

(defn mod-exp [mod-p a n]
    (reduce #(mod (* %1 (int (Math/pow (first %2) (mod (second %2) 2)))) mod-p) 1
        (take-while #(> (second %) 0)
        (iterate (fn [[x n-exp]] [(mod (* x x) mod-p) (quot n-exp 2)]) [a n]))))

; start out with Fermat's Primality Test
; (basically just uses Fermat's Little Theorem)
; use this to build a lazy prime sequence


; useful to have a slightly better random integer
; generator that generates numbers >= min and < max
(defn rand-int-range [min max]
    (+ min (rand-int (- max min))))

(defn rand-ints-range [min max]
    (lazy-seq
        (cons (rand-int-range min max)
            (rand-ints-range min max))))

; Note that we have tried to make Fermat's Test
; slightly more robust by performing the check
; n-times (default is n-times = 10)

; TO DO: can improve this slightly by including
; a check of whether base is relatively prime to
; p

(defn fermat-test 
    ([p] (fermat-test p 10))
    ([p n-times]
        (if (= p 2)
            true
            (if (every? zero? 
                (for [base (take n-times (rand-ints-range 2 p))] (dec (mod-exp p base (dec p)))))
                true
                false
            ))))

; Miller-Rabin Algorithm
;
; Improved (and widely used) primality algorithm
; ... to do ... understand how it works....

(defn power2-factor [p]
    (let [pows (take-while integer? (iterate #(/ % 2) p))]
        [(last pows) (dec (count pows))]))

(defn witness [base p m b]
    (let [mod-pow (mod-exp p base m)]
        (if (or (= mod-pow 1) (= mod-pow (dec p)))
            true
            (let [last-pow (last (take b (take-while #(and (not= % 1) (not= % -1)) (iterate #(mod (* % %) p) mod-pow))))]
                (cond
                    (= last-pow 1) false
                    (= last-pow (dec p)) true
                    :else false)))))

(defn miller-rabin
    ([p] (miller-rabin p 4))
    ([p n-times]
        (if (= p 2)
            true
            (if (even? p)
                false
                (let [[m b] (power2-factor (dec p))]
                    (if (every? true?
                        (for [base (take n-times (rand-ints-range 2 p))]
                            (witness base p m b)))
                        true
                        false))))))

; AKS Algorithm
;
; Might also be interesting to implement the
; deterministic primality algorithm of Agarwal et. al.

; returns the first n primes using a probabilistic primality test
; by default we use the rabin-miller test
; TO DO (should add an option for which primality test to use!!)
(defn primes 
    ([n] (primes n 10))
    ([n k] (primes n k miller-rabin))
    ([n k primality-test] (take n (filter #(primality-test % k) (iterate inc 2)))))

; compare this generator to a slow but guaranteed correct generator
(def nprimes 1000)
(= (take nprimes (filter is-prime? (iterate inc 2))) (primes nprimes 3 fermat-test))

(subvec (vec (primes 1987)) 1980 1986)
(subvec (vec (take 1987 (filter is-prime? (iterate inc 2)))) 1980 1986)

; returns the first n pairs of twin primes
(defn twin-primes [n]
    (take n (filter #(= (first %) (- (second %) 2)) 
        (partition 2 1 (filter fermat-test (iterate inc 2))))))

((fn [x] (take x (map first (iterate (fn [j k] [k (+ j k)]) [0 1])))) 3)

(reduce #(cons %1 (first (last (take-while seq? (iterate first %2))))) [] [1 2 3 4])

((fn custom-flatten [x]
    (if ((comp not coll?) x)
        [x]
        (if (zero? (count x))
            []
            (concat (custom-flatten (first x)) (custom-flatten (rest x)))))) [[[4 2] 3 [19 20 [30]]] [2 13 1000]])

(defn flatten2 [x] 
    (filter (complement sequential?) (tree-seq sequential? seq x)))

(defn flatten3 [x]
    (reduce (fn self [xs e]
                (concat xs
                    (if (coll? e)
                        (reduce self '() e)
                        (list e)))) '() x)

(defn splitter [x]
    (loop [x x repeats-x []]
    (if (> (count x) 0)
        (let [split-x (split-with (partial = (first x)) x)]
            (recur (second split-x) (conj repeats-x (first split-x))))
        repeats-x)))

(defn custom-max
    ([x] x)
    ([x & more] (reduce #(if (> %1 %2) %1 %2) x more)))
;asfsd

#(apply concat (reverse (split-at (mod %1 (count %2)) %2)))

(((fn [f] (comp (partial apply f) #(vec (reverse %&)))) >) 7 8)


(set ((fn [x] (vals (reduce #(assoc %1 (class %2) (concat (%1 (class %2)) [%2])) {} x))) [1 :a 2 :b 3 :c]))
;

; THIS IS ALL SCRATCH BELOW THIS POINT!!
;
;

(reduce #(if (< (last %1) %2) (concat %1 [%2]) [%2]) [1] [0 1 2 3 0 4 5])
;
;
(defn inc-subseq [x]
    (map (partial reduce 
            #(if (sequential? %1)
                (if (< (last %1) %2) (concat %1 [%2]) %1)
                (list %1)))
            (partition-all (count x) 1 x)))


(defn inc-subseq [x]
    (loop [x x best []]
        (if (zero? (count x))
            best
            (let [inc-seq (apply anchored-longest-inc x) length (count inc-seq)]
                (recur (drop length x) (if (> length (count best)) inc-seq best))))))

;
(reduce #(if (< (last (vec %1)) %2) (concat (vec %1) [%2]) (vec %1)) [1 0 1 2 3 0 4 5])
;
;
(reduce #(concat %1 [(last %2)]) [] (take-while #(< (first %) (second %)) (partition 2 1 [1 0 8 -3 10])))
;
;
(map (partial reduce #(concat %1 [(last %2)])) ((juxt take-while drop-while) #(< (first %) (second %)) (partition 2 1 [8 3 0 4 5])))
;
(split-with #(not (>= (first %) (second %))) (partition 2 1 [8 3 0 4 5]))

;
(defn anchored-longest-inc
    ([x] [x])
    ([x y] (if (< x y) [x y] [x]))
    ([x y & more]
        (if (< x y)
            (concat [x] (apply anchored-longest-inc y more))
            [x])))

(defn inc-subseq [x]
    (loop [x x best []]
        (if (zero? (count x))
            best
            (let [inc-seq (reduce #(concat (drop-last %1) %2) [] (take-while #(< (first %) (second %)) (partition 2 1 x))) 
                length (count inc-seq)]
                (recur (drop (max 1 length) x) (if (> length (count best)) inc-seq best))))))

(reduce #(concat (drop-last %1) %2) [nil] (take-while #(< (first %) (second %)) (partition 2 1 [-3 -1])))    

(defn hand-partition [n x]
    (loop [x x part '()]
        (if (< (count x) n)
            part
            (recur (drop n x) (concat part [(take n x)])))))

((fn [f x]
  (reduce #(assoc %1 (f %2) (concat (%1 (f %2) []) [%2])) {} x)) #(> % 5) [1 3 6 8])

(assoc {} true [3])

(defn base-digits [n b]
  (loop [n n digits []]
    (let [remainder (rem n b)]
        (if (= remainder n)
          (concat [n] digits)
          (recur (int (/ n b)) (concat [remainder] digits))))))




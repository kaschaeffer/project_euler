; Project Euler problems in Clojure

; Problem 1
; 
; Sum of all numbers that are multiples of 3 or 5 and
; are less than cutoff
(defn sum35 [cutoff]
    (apply + (for [x (range 0 cutoff) 
        :let [y (mod x 3) z (mod x 5)] 
        :when (or (= 0 y) (= 0 z))] 
        x))

; Problem 2
;
; Sum of all all Fibonacci numbers whose values are 
; less than cutoff
(defn even_fib_sum [cutoff]
    (apply + (for [x (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))
        :while (< x cutoff) 
        :when (even? x)]
        x)))

; Problem 3
; 
; Find the largest prime factor of a given number

(defn prime-factors [n]
    (loop [n n factors []]
        (if (= n 1)
            factors
        (let [divisor (first (take 1 (filter #(zero? (rem n %)) 
                (iterate inc (if factors 2 (last factors))))))] 
            (recur (/ n divisor) (conj factors divisor))))))

(defn prime-factors-simple [n]
    (vec (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} (prime-factors n))))

(defn is-prime? [n]
    (if (= 1 (count (prime-factors n)))
        true
        false))

; Problem 4
;
; Find the largest palindromic number that is equal to the 
; product of two n-digit numbers
;
; Note that this implementation is not terribly efficient -- for
; n-digit=4 it is already quite slow...

(defn reverse-digits [n]
    (Integer. (reduce str (reverse (map #(Character/digit % 10) (str n)))))
    )

(defn is-palindrome? [n]
    (= (reverse-digits n) n)
    )

(defn is-palindrome? [n]
    (= (reverse (str n)) (seq (str n))))

(defn biggest-pal [n-digits]
    (let [max-num (Integer. (reduce str (repeat n-digits 9)))]
        (take 1 (filter is-palindrome? 
            (sort > (for [x (range max-num (int (Math/pow 10 (dec n-digits))) -1) 
                        y (range max-num (dec x) -1)] (* x y)))))))

(defn biggest-pal2 [n-digits]
    (let [max-num (Integer. (reduce str (repeat n-digits 9)))]
        (reduce max (filter is-palindrome? 
            (for [x (range max-num (int (Math/pow 10 (dec n-digits))) -1) 
                        y (range max-num (dec x) -1)] (* x y))))))

; Note that we can do a little better than this
; by moving along diagonals and lazily obtaining a decreasing
; list of products of n-digit numbers
;
; NOTE: THIS IS TOO NAIVE!!! it does NOT work!!

(def max-num 9)

(defn next-diag [[a b]] 
    (let [absum (+ a b)]
        (if (= a max-num) 
            (if (even? absum) 
                [(/ absum 2) (dec (/ absum 2))] 
                [(/ (dec absum) 2) (/ (dec absum) 2)])
            [(inc a) (dec b)])))

(take 10 (iterate next-diag [9 9] 9))

; Problem 5
;
; Find the smallest number that is divisible by all of
; the numbers 1 through n
;
; We start by doing this the naive iterative way
;
; There should also be a method that uses the Chinese
; Remainder Theorem

(defn is-div? [n x]
    (if (= 1 n)
        true
        (and (= 0 (mod x n)) (is-div? (dec n) x))))

(defn smallest-div [n]
    (first (take 1 (filter (partial is-div? n) (iterate inc 1)))))

; iterative way of stepping to get the LCM
; the implementation below is crazy slow...

(defn lcm [nums]
    (loop [new-nums nums]
        (if (= 1 (count (set new-nums)))
            (first new-nums)
        (let [min-val (apply min new-nums) min-ind (.indexOf new-nums min-val)] 
            (recur (assoc new-nums min-ind (+ min-val (nth nums min-ind)))))))) 

; using factorization
; we make use of a hash-map to compute the lcm after factorizing each number
(defn smallest-div [n]
    (let [factor-lists (apply concat (for [x (range 2 (inc n))] (prime-factors-simple x)))]
        (reduce #(* %1 (int (Math/pow (first %2) (second %2)))) 1
            (reduce 
                #(if (> (second %2) (%1 (first %2) 0))
                    (assoc %1 (first %2) (second %2))
                    %1)
                {} factor-lists))))

; another interesting approach is to use Euclid's Algorithm iteratively
; TO DO!





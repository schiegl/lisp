(defn factorial (n)
  (if (< n 1)
      1
      (* n (factorial (- n 1)))))

(println "10th_factorial:" (factorial 100))

(defn fib (n)
  ((defn _fib (a b i)
    (if (< i 1)
        b
        (_fib b (+ a b) (- i 1))))
   (_fib 0 1 n)))

(println "10th_fibonacci:" (fib 10))

(defn nand (vals)
  (not (and vals)))

(defn or (vals)
  (and (not vals)))



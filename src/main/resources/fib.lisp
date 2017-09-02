;; nth fibonacci number
(defn fib (params n)
  (do
    (defn go (params a b i)
      (if (< i 1)
        b
        (go b (+ a b) (- i 1))))
    (go 0 1 n)))

(fib 10)

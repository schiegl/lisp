;; nth factorial number
(defn fac (params n)
  (if (< n 2)
      1
      (* n (fac (- n 1)))))

(fac 7)

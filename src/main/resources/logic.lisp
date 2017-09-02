(defn or (params a b)
  (not (and (not a) (not b))))

(defn -> (params a b)
  (or (not a) b))
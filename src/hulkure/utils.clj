(ns hulkure.utils)

(defn filter-first [pred coll]
  (first (filter pred coll)))

(defn removev [v index]
  (vec (concat (subvec v 0 index)
               (subvec v (inc index)))))

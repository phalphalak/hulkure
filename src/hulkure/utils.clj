(ns hulkure.utils)

(defn filter-first [pred coll]
  (first (filter pred coll)))
(ns hulkure.test.board
  (:use clojure.test)
  (:use [hulkure.board]))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(with-private-fns [hulkure.board [coordinates-to-index]]

  (def board )

  (deftest make-board-tests
    (testing "empty board"
      (let [board (make-board 20 15)]
        (is (= (board :width) 20))
        (is (= (board :height) 15))
        (is (= (board :fields) (vec (take 300 (repeat nil))))))))

  (deftest coordinates-to-index-tests
    (let [board (assoc-in (make-board 20 15) [:fields] (vec (range 300)))]
      (testing "valid coordintates"
        (is (= (coordinates-to-index board 0 0) 0))
        (is (= (coordinates-to-index board 12 0) 12))
        (is (= (coordinates-to-index board 19 0) 19))
        (is (= (coordinates-to-index board 0 1) 20))
        (is (= (coordinates-to-index board 19 14) 299)))
      (testing "invalid coordinates"
        (is (thrown? AssertionError (coordinates-to-index board -1  0)))
        (is (thrown? AssertionError (coordinates-to-index board  0 -1)))
        (is (thrown? AssertionError (coordinates-to-index board -1 -1)))
        (is (thrown? AssertionError (coordinates-to-index board 20  0)))
        (is (thrown? AssertionError (coordinates-to-index board 21  1)))
        (is (thrown? AssertionError (coordinates-to-index board  0 15)))
        (is (thrown? AssertionError (coordinates-to-index board  1 16)))
        (is (thrown? AssertionError (coordinates-to-index board 20 15)))
        (is (thrown? AssertionError (coordinates-to-index board 21 16))))))

  (deftest add-figure-tests
    (let [board (make-board 10 10)]
      (testing "figures with coordinates"
        (let [figure-1 {:x 3, :y 2}, figure-2 {:x 5, :y 2}]
          (let [board (add-figure (add-figure board figure-1) figure-2)]
            (is (= (board :figures) [{:x 3, :y 2, :id 0}, {:x 5, :y 2, :id 1}]))
            (is (= (field board 3 2) {:figure 0}))
            (is (= (field board 5 2) {:figure 1})))))
      (testing "figures withut coordinates"
        (let [figure-1 {}, figure-2 {}]
          (is (= (board :figures) [{:x 3, :y 2, :id 0}, {:x 5, :y 2, :id 1}]))))))

  (comment
    (deftest figure-tests
      (let [board (add-figure
                   (add-figure (set-field (set-field
                                           (make-board 10 10) 3 2 {}) 5 2 {})
                               {:x 3, :y 2}) {:x 5, :y 2})]
        (testing "figure by id"
          (is (= (figure board 0) {:x 3, :y 2, :id 0}))
          (is (= (figure board 1) {:x 5, :y 2, :id 1}))
          (is (= (figure board 2) :foo)))
        (testing "figure by coordinates"
          (is (= (figure board 3 2) {:x 3, :y 2, :id 0}))
          (is (= (figure board 5 2) {:x 5, :y 2, :id 1}))
          (is (= (figure board 1 1) nil)))
        ))))

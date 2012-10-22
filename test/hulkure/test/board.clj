(ns hulkure.test.board
  (:use clojure.test)
  (:use [hulkure.board])
  (:require [hulkure.board :as board]))

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
    (let [board (board/load "test/fixtures/map.json")]
      (testing "figures with coordinates"
        (let [figure-1 {:x 3, :y 1}
              figure-2 {:x 7, :y 2}
              figure-3 {:x 7, :y 2}
              board (add-figure (add-figure (add-figure board
                                                        figure-1)
                                            figure-2)
                                figure-3)]
          (is (= (board :figures)
                 [{:x 3, :y 1, :id 0},
                  {:x 7, :y 2, :id 1},
                  {:x 7, :y 2, :id 2}]))
          (is (= (get-figures-at board 3 1)
                 [{:x 3, :y 1, :id 0}]))
          (is (= (get-figures-at board 7 2)
                 [{:x 7, :y 2, :id 1},
                  {:x 7, :y 2, :id 2}]))))
      (testing "figures without coordinates"
        (let [figure-1 {}
              figure-2 {}
              board (add-figure (add-figure board figure-1) figure-2)]
          (is (= (board :figures) [{:id 0}, {:id 1}]))))))

  (deftest relative-movement-to-offset-tests
    (testing "forward movement"
      (is (= (relative-movement-to-offset :forward :north) [ 0 -1]))
      (is (= (relative-movement-to-offset :forward :west)  [-1  0]))
      (is (= (relative-movement-to-offset :forward :south) [ 0  1]))
      (is (= (relative-movement-to-offset :forward :east)  [ 1  0])))
    (testing "backward movement"
      (is (= (relative-movement-to-offset :backward :north) [ 0  1]))
      (is (= (relative-movement-to-offset :backward :west)  [ 1  0]))
      (is (= (relative-movement-to-offset :backward :south) [ 0 -1]))
      (is (= (relative-movement-to-offset :backward :east)  [-1  0])))
    (testing "left movement"
      (is (= (relative-movement-to-offset :left :north) [-1  0]))
      (is (= (relative-movement-to-offset :left :west)  [ 0  1]))
      (is (= (relative-movement-to-offset :left :south) [ 1  0]))
      (is (= (relative-movement-to-offset :left :east)  [ 0 -1])))
    (testing "right movement"
      (is (= (relative-movement-to-offset :right :north) [ 1  0]))
      (is (= (relative-movement-to-offset :right :west)  [ 0 -1]))
      (is (= (relative-movement-to-offset :right :south) [-1  0]))
      (is (= (relative-movement-to-offset :right :east)  [ 0  1])))
    (testing "forward-left movement"
      (is (= (relative-movement-to-offset :forward-left :north) [-1 -1]))
      (is (= (relative-movement-to-offset :forward-left :west)  [-1  1]))
      (is (= (relative-movement-to-offset :forward-left :south) [ 1  1]))
      (is (= (relative-movement-to-offset :forward-left :east)  [ 1 -1])))
    (testing "forward-right movement"
      (is (= (relative-movement-to-offset :forward-right :north) [ 1 -1]))
      (is (= (relative-movement-to-offset :forward-right :west)  [-1 -1]))
      (is (= (relative-movement-to-offset :forward-right :south) [-1  1]))
      (is (= (relative-movement-to-offset :forward-right :east)  [ 1  1])))
    (testing "backward-left movement"
      (is (= (relative-movement-to-offset :backward-left :north) [-1  1]))
      (is (= (relative-movement-to-offset :backward-left :west)  [ 1  1]))
      (is (= (relative-movement-to-offset :backward-left :south) [ 1 -1]))
      (is (= (relative-movement-to-offset :backward-left :east)  [-1 -1])))
    (testing "backward-right movement"
      (is (= (relative-movement-to-offset :backward-right :north) [ 1  1]))
      (is (= (relative-movement-to-offset :backward-right :west)  [ 1 -1]))
      (is (= (relative-movement-to-offset :backward-right :south) [-1 -1]))
      (is (= (relative-movement-to-offset :backward-right :east)  [-1  1])))
    )

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

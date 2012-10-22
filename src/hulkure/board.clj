(ns hulkure.board
  (:require [clojure.data.json :as json]))

(defn make-board [width height]
  {:width width,
   :height height,
   :fields (vec (take (* width height) (repeat nil))),
   :figures [],
   :round 0,
   :current-player 0,
   :figure-templates {}})

(defn load [path]
  (json/read-str (slurp path) :key-fn keyword))

(defn- coordinates-to-index [board x y]
  {:pre [(<= 0 x) (< x (board :width)) (<= 0 y) (< y (board :height))]}
  (+ (* y (board :width)) x))

(defn get-field [board x y]
  ((board :fields) (coordinates-to-index board x y)))

(defn- set-field [board x y value]
  (assoc-in board [:fields (coordinates-to-index board x y)] value))

(defn- get-indexed-figure [board id]
  (filter (fn[index figure] (= (figure :id) id)
            (map-indexed vector (board :figures)))))

(defn get-figure-by-id [board id]
  (second (get-indexed-figure board id)))

(defn get-figure-index [board id]
  (first (get-indexed-figure board id)))

(defn get-figures-at [board x y]
  (filterv (fn [figure] (= [x y]
                           (mapv figure [:x :y])))
           (board :figures)))

(defn set-figure [board id new-figure]
  (assoc-in board [:figures (get-figure-index board id)] new-figure))

(defn update-figure [board id attributes]
  (set-figure board id (merge (get-figure-by-id id) attributes)))

(defn place-figure [board id x y]
  {:pre [(not (nil? (get-field board x y)))]}
  (update-figure board id {:x x :y y}))

(defn next-available-figure-id [board]
  (if (empty? (board :figures))
    0
    (inc (apply max (map :id (board :figures))))))

(defn add-figure [board figure]
  (let [id (next-available-figure-id board)]
    (assoc-in board
              [:figures (count (board :figures))]
              (assoc figure :id id))))

;; movement related
(def movement-offset
  {:forward  [0 -1]
   :backward [0  1]
   :left     [-1 0]
   :right    [ 1 0]
   :forward-left   [-1 -1]
   :forward-right  [ 1 -1]
   :backward-left  [-1  1]
   :backward-right [ 1  1]})

(def rotation_matrix {:north [[ 1  0] [ 0  1]]
                      :west  [[ 0  1] [-1  0]]
                      :east  [[ 0 -1] [ 1  0]]
                      :south [[-1  0] [ 0 -1]]})

(def directions (keys movement-offset))

(defn- matrix-vector-mult [matrix vector]
  (map (fn [row] (reduce + (map * row vector))) matrix))

(defn relative-movement-to-offset [movement heading]
  (matrix-vector-mult (rotation_matrix heading) (movement-offset movement)))

(defmulti figure (fn [board & args] ()))

(defmethod figure :by-id [board id]
  ((board :figures) id))

(defmethod figure :by-coordinates [board x y]
  (let [id (get-field x y)] (figure id)))

(defn template [board name]) ;; multi name or figure

(comment
  (defn- flood-fill [grid x y replacement]
    (let [grid-ref (atom grid)
          target (get-in grid [y x])
          stack (atom [])]
      (reset! stack (conj @stack [x y]))
      (while (not-empty @stack)
        (let [[x y] (peek @stack)]
          (swap! stack pop)
          (cond (= (get-in @grid-ref [y x]) target)
                (do
                  (reset! grid-ref (assoc-in @grid-ref [y x] replacement))
                  (cond (> x 0)
                        (reset! stack (conj @stack [(dec x) y])))
                  (cond (> y 0)
                        (reset! stack (conj @stack [x (dec y)])))
                  (cond (< x (dec (count (first @grid-ref))))
                        (reset! stack (conj @stack [(inc x) y])))
                  (cond (< y (dec (count @grid-ref)))
                        (reset! stack (conj @stack [x (inc y)])))))))
      @grid-ref))

  (defn upper-case? [str] (boolean (re-matches #"[A-Z]" str)))

  (defn load-board [path]
    (let [char-map (atom (mapv vec (vec (.split (slurp path) "\n"))))
          tile-count (atom 0)]
      (doseq [x (range (count (first @char-map))), y (range (count @char-map))]
        (let [val (get-in @char-map [y x])
              void? (= val \.)]
          (if (instance? Character val)
            (do
              (reset! char-map (flood-fill @char-map x y (if void?
                                                           nil
                                                           [@tile-count (upper-case? (str val))])))
              (if (not void?)
                (swap! tile-count inc))))
          (comment (let [board (atom (make-board (count (first char-map)) (count char-map)))]
                     (doseq [ [row y] (map vector char-map (range))
                              [val x] (map vector row (range))]
                       (reset! board (set-field @board x y [])))
                     @board))))
      (let [board (atom (make-board (count (first @char-map)) (count @char-map)))]
        (doseq [[row y] (map vector @char-map (range))
                [[tile-id room?] x] (map vector row       (range))]
          (cond tile-id
                (reset! board (set-field @board x y {:tile-id tile-id, :room room?})))
          )
        @board)))
)
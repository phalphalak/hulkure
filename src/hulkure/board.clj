(ns hulkure.board
  (:require [clojure.data.json :as json])
  (:use [hulkure.utils :only [filter-first
                              removev]]))

(defn make-board []
  {:fields [], ; contains fields. The keys :x and :y are handled like unique keys
   :figures [],
   :tiles {}, ; contains mapping of tile id to tile-info hash
   :round 0,
   :current-player 0})

(defn load-board [path]
  (json/read-str (slurp path) :key-fn keyword))

;; tiles

(defn set-tile [board id tile-info]
  (assoc-in board [:tiles id] tile-info))

(defn get-tile [board id]
  (get-in board [:tiles id]))

(defn get-tile-ids [board]
  (keys (board :tiles)))

;; fields

(defn- get-field-with-index [board x y]
  "returns [index field]"
  (or (filter-first #(and (= x ((second %) :x))
                          (= y ((second %) :y)))
                    (map-indexed vector (board :fields)))
      [nil nil]))

(defn- get-field-index [board x y]
  (first (get-field-with-index board x y)))

(defn get-field
  ([board xy]
     (get-field board (xy :x) (xy :y)))
  ([board x y]
     (last (get-field-with-index board x y))))

(defn remove-field
  "remove field at coordinates x and y. Warning: does not handle figures at specified coordinates."
  ([board xy]
     (remove-field board (xy :x) (xy :y)))
  ([board x y]
     (let [[index field] (get-field-with-index board x y)]
       (if index
         (assoc board :fields (removev (board :fields)
                                       index))
         board))))

(defn set-field [board field]
  "Update or add field if it does not exist"
  (let [index (or (get-field-index board (field :x) (field :y))
                  (count (board :fields)))]
    (assoc-in board [:fields index] field)))

(defn get-tile-fields [board tile-id]
  (filterv #(= tile-id (% :tile-id)) (board :fields)))

;; figures

(defn get-next-available-figure-id [board]
  (if (empty? (board :figures))
    0
    (inc (apply max (map :id (board :figures))))))

(defn- get-figure-with-index [board id]
  "returns [index field]"
  (or (filter-first #(= id (% :id)) (board :figures))
      [nil nil]))

(defn- get-figure-index [board id]
  (second (get-figure-with-index board id)))

(defn get-figure [board id]
  (first (get-figure-with-index board id)))

(defn next-available-figure-id [board]
  (if (empty? (board :figures))
    0
    (inc (apply max (map :id (board :figures))))))

(defn set-figure [board figure]
  "updates or adds a figure. If no :id is given the next available one will be associated with the figure"
  (let [[index figure] (if (figure :id)
                         [(or (get-figure-index board (figure :id))
                              (count (board :figures)))
                          figure]
                         [(count (board :figures)) (assoc figure :id (next-available-figure-id board))])]
    (assoc-in board [:figures index] figure)))

(defn get-figures-at [board x y]
  (filterv (fn [figure] (= [x y]
                           (mapv figure [:x :y])))
           (board :figures)))

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

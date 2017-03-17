(ns super-tetris.initialization
  (:use [super-tetris.shape :only [create-shape move-up move-to-center]])
  (:require [super-tetris.config :refer [initial-square-count vertical-count horizontal-count]]))

(def some-shape (move-up (move-to-center (create-shape initial-square-count))))

;;;functions related to setting up the game state

(defn- calculate-dimensions [horizontal-count vertical-count sq-size gap-width]
  [(+ (* horizontal-count sq-size) (* horizontal-count gap-width) (- gap-width))
   (+ (* vertical-count sq-size) (* vertical-count gap-width) (- gap-width))])

(defn- create-square [h-pos v-pos sq-size gap-width]
  {:x (+ (* h-pos sq-size) (* h-pos gap-width))
   :y (+ (* v-pos sq-size) (* v-pos gap-width))
   :shown false})

(defn- gen-game-map [horizontal-count vertical-count sq-size gap-width]
  (->> (for [i (range horizontal-count)
             k (range vertical-count)]
         (create-square i k sq-size gap-width))
       (partition vertical-count)
       (map #(vec %))
       (vec)))

;; used for debugging
(defn gen-bottom-rows [count]
  (let [random-column (rand-int horizontal-count)]
    (vec (apply concat (for [x (filter #(not= % random-column) (take horizontal-count (range)))
                             :let [y (dec vertical-count)]]
                         (map (fn [row] [x (- y row)]) (take count (range))))))))

(defn init-game-state [& args]
  (let [[width height] (apply calculate-dimensions args)]
    {:game-map (apply gen-game-map args)
     :width width
     :height height
     :curr-shape some-shape
     :existing-shapes (gen-bottom-rows 2)
     :square-count initial-square-count
     :y-pos 0}))

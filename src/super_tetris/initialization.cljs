(ns super-tetris.initialization)

;;functions related to setting up the game state
(defn calculate-dimensions [vertical-count horizontal-count sq-size gap-width]
  [(+ (* vertical-count sq-size) (* vertical-count gap-width) (- gap-width))
   (+ (* horizontal-count sq-size) (* horizontal-count gap-width) (- gap-width))])

(defn create-square [h-pos v-pos sq-size gap-width]
  {:x (+ (* h-pos sq-size) (* h-pos gap-width))
   :y (+ (* v-pos sq-size) (* v-pos gap-width))
   :shown false})

(defn gen-game-map [vertical-count horizontal-count sq-size gap-width]
  (->> (for [i (range horizontal-count)
             k (range vertical-count)]
         (create-square i k sq-size gap-width))
       (partition vertical-count)
       (map #(vec %))
       (vec)))

(defn init-game-state [& args]
  (let [[height width] (apply calculate-dimensions args)]
    {:game-map (apply gen-game-map args)
     :width width
     :height height}))

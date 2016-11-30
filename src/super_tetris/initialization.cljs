(ns super-tetris.initialization)

(def some-shape [[0 0] [0 1] [0 2] [1 0]])                ; г-shape

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

(defn init-game-state [& args]
  (let [[width height] (apply calculate-dimensions args)]
    {:game-map (apply gen-game-map args)
     :width width
     :height height
     :curr-shape some-shape
     :existing-shapes []
     :y-pos 0}))

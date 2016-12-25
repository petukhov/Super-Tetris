(ns super-tetris.event-handling
  (:require [clojure.walk :refer [prewalk]]))

(declare move-down)

(def bottom-y 9)

(defn apply-shape [game-map shape]
  (letfn [(show-square [game-map curr]
            (if (neg? (curr 1))
              game-map
              (update-in game-map (conj curr :shown) (constantly true))))]
    (reduce show-square game-map shape)))

(defn clear-map [game-map]
  (prewalk #(if (true? (:shown %))
             (assoc % :shown false)
             %) game-map))

(defn update-game-map-with-shape [game-map shape reached-bottom?]
  (let [game-map (if-not reached-bottom?
                   (clear-map game-map)
                   game-map)]
    (apply-shape game-map shape)))

(defn update-game-map-with-existing-shapes [game-map existing-shapes]
  (apply-shape game-map existing-shapes))

(defn get-left-side-x [shape]
  (first (apply min-key first shape)))

(defn get-right-side-x [shape]
  (first (apply max-key first shape)))

(defn get-bottom-y [shape]
  (second (apply max-key second shape)))

(defn get-top-y [shape]
  (second (apply min-key second shape)))

(defn will-touch-existing-shapes? [shape existing-shapes]
  (not (empty? (for [s1 shape
                     s2 existing-shapes
                 :when (= s1 s2)]
             s1))))

(defn reached-bottom? [shape] (=  bottom-y (get-bottom-y shape)))

(defn will-stop? [shape existing-shapes]
  (or (reached-bottom? shape)
      (will-touch-existing-shapes? (move-down shape) existing-shapes)))

(defn make-new-shape []
  [[0 0] [0 1] [0 2] [1 1]])

(defn move-to-center [shape]
  (let [shape-width (- (get-right-side-x shape) (get-left-side-x shape))
        offset (js/Math.floor (- 5 (/ shape-width 2)))]
    (map #(update % 0 + offset) shape)))

(defn move-up [shape]
  (let [shape-height (inc (get-bottom-y shape))]
    (map #(update % 1 - shape-height) shape)))

(defn move-down [shape] (map #(update % 1 inc) shape))
(defn move-left [shape] (map #(update % 0 dec) shape))
(defn move-right [shape] (map #(update % 0 inc) shape))

(defn forward-normalizer [x-offset y-offset [x y]]
  [(- x x-offset) (- y y-offset)])

(defn backward-normalizer [x-offset y-offset [x y]]
  [(+ x x-offset) (+ y y-offset)])

(defn get-offsets [shape]
  (let [top-y (get-top-y shape)
        left-x (get-left-side-x shape)
        height (- (get-bottom-y shape) (get-top-y shape))
        width (- (get-right-side-x shape) (get-left-side-x shape))
        x-offset (+ left-x (.round js/Math (quot width 2)))
        y-offset (+ top-y (.round js/Math (quot height 2)))]
    (prn "leftx and topy: " left-x top-y)
    (prn "width and height: " height width)
    (prn "x-offset and y-offset: " x-offset y-offset)
    [x-offset y-offset]))

(defn normalize [shape normalizer [x-offset y-offset]]
  (vec (map (partial normalizer x-offset y-offset) shape)))

(defn rotate-normalized [shape]
  (map (fn [[x y]] [(- y) x]) shape))

(defn rotate [shape]
  (let [offsets (get-offsets shape)]
    #_(prn offsets)
    (-> shape
        (normalize forward-normalizer offsets)
        (rotate-normalized)
        (normalize backward-normalizer offsets))))

(defn move-shape [shape dir existing-shapes]
  (case dir
    :left [(if (or (zero? (get-left-side-x shape))
                   (will-touch-existing-shapes? (move-left shape) existing-shapes))
             shape
             (move-left shape)) false]
    :right [(if (or (= 9 (get-right-side-x shape))
                    (will-touch-existing-shapes? (move-right shape) existing-shapes))
              shape
              (move-right shape)) false]
    :down (if (will-stop? shape existing-shapes)
            [(move-up (move-to-center (make-new-shape))) true shape]
            [(move-down shape) false])
    :rotate [(rotate shape) false]
    :default [shape false]))

(defn update-existing-shapes-if-needed [existing-shapes reached-bottom? shape]
  (if reached-bottom?
    (vec (concat existing-shapes shape))
    existing-shapes))

(defn transform-state [{:keys [game-map curr-shape existing-shapes] :as state} dir]
  (let [[shape-updated reached-bottom? old-shape] (move-shape curr-shape dir existing-shapes)
        updated-with-curr-shape (update-game-map-with-shape game-map shape-updated reached-bottom?)
        updated-with-everything (update-game-map-with-existing-shapes updated-with-curr-shape existing-shapes)]
    (assoc state
      :game-map updated-with-everything
      :curr-shape shape-updated
      :existing-shapes (update-existing-shapes-if-needed existing-shapes reached-bottom? old-shape))))

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :left-key {:should-rerender? true, :new-state (transform-state state :left)}
    :right-key {:should-rerender? true, :new-state (transform-state state :right)}
    :rotate-key {:should-rerender? true, :new-state (transform-state state :rotate)}
    :down-key {:should-rerender? true, :new-state (transform-state state :down)}
    :tick {:should-rerender? true, :new-state (transform-state state :down)}
    :nothing {:should-rerender? false, :new-state state}
    :default {:should-rerender? false, :new-state state}))

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

(defn update-game-map-with-shape [game-map {:keys [squares center]} reached-bottom?]
  (assert squares ":squares is not defined")
  (let [game-map (if-not reached-bottom?
                   (clear-map game-map)
                   game-map)]
    (apply-shape game-map squares)))

(defn update-game-map-with-existing-shapes [game-map existing-shapes]
  (apply-shape game-map existing-shapes))

(defn get-left-side-x [{:keys [squares center]}]
  (assert squares ":squares is not defined")
  (first (apply min-key first squares)))

(defn get-right-side-x [{:keys [squares center]}]
  (assert squares ":squares is not defined")
  (first (apply max-key first squares)))

(defn get-bottom-y [{:keys [squares center] :as shape}]
  (assert squares ":squares is not defined")
  (second (apply max-key second squares)))

(defn get-top-y [{:keys [squares center]}]
  (assert squares ":squares is not defined")
  (second (apply min-key second squares)))

(defn will-touch-existing-shapes? [{:keys [squares center]} existing-shapes]
  (assert squares ":squares is not defined")
  (not (empty? (for [s1 squares
                     s2 existing-shapes
                 :when (= s1 s2)]
             s1))))

(defn reached-bottom? [shape] (=  bottom-y (get-bottom-y shape)))

(defn will-stop? [shape existing-shapes]
  (or (reached-bottom? shape)
      (will-touch-existing-shapes? (move-down shape) existing-shapes)))

(defn find-center [shape]
  (let [top-y (get-top-y shape)
        left-x (get-left-side-x shape)
        height (- (get-bottom-y shape) (get-top-y shape))
        width (- (get-right-side-x shape) (get-left-side-x shape))
        x-offset (+ left-x (.round js/Math (quot width 2)))
        y-offset (+ top-y (.round js/Math (quot height 2)))]
    (prn "leftx and topy: " left-x top-y)
    (prn "width and height: " width height)
    (prn "x-offset and y-offset: " x-offset y-offset)
    [x-offset y-offset]))

(defn make-new-shape []
  (let [squares [[0 0] [0 1] [0 2] [0 3]]]
    {:squares squares :center (find-center {:squares squares})}))

(defn move-to-center [{:keys [squares center] :as shape}]
  (assert squares ":squares is not defined")
  (let [shape-width (- (get-right-side-x shape) (get-left-side-x shape))
        offset (js/Math.floor (- 5 (/ shape-width 2)))]
    {:squares (map #(update % 0 + offset) squares) :center (update center 0 + offset)}))


(defn move-up [{:keys [squares center] :as shape}]
  (assert squares ":squares is not defined")
  (let [shape-height (inc (get-bottom-y shape))]
    {:squares (map #(update % 1 - shape-height) squares) :center (update center 1 - shape-height)}))

(defn move-down [{:keys [squares center]}]
  (assert squares ":squares is not defined")
  {:squares (map #(update % 1 inc) squares) :center (update center 1 inc)})

(defn move-left [{:keys [squares center]}]
  (assert squares ":squares is not defined")
  {:squares (map #(update % 0 dec) squares) :center (update center 0 dec)})

(defn move-right [{:keys [squares center]}]
  (assert squares ":squares is not defined")
  {:squares (map #(update % 0 inc) squares) :center (update center 0 inc)})


(defn forward-normalizer [x-offset y-offset [x y]]
  [(- x x-offset) (- y y-offset)])

(defn backward-normalizer [x-offset y-offset [x y]]
  [(+ x x-offset) (+ y y-offset)])

(defn normalize [{:keys [squares center]} normalizer [x-offset y-offset]]
  {:squares (vec (map (partial normalizer x-offset y-offset) squares)) :center center})

(defn rotate-normalized [{:keys [squares center]}]
  {:squares (map (fn [[x y]] [(- y) x]) squares) :center center})

(defn rotate [shape]
  (let [offsets (:center shape)]
    #_(prn offsets)
    (-> shape
        (normalize forward-normalizer offsets)
        (rotate-normalized)
        (normalize backward-normalizer offsets))))

(defn move-shape [shape dir existing-shapes]
  "event dispatcher for moving the shape"
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
    (vec (concat existing-shapes (:squares shape)))
    existing-shapes))

(defn transform-state [{:keys [game-map curr-shape existing-shapes] :as state} dir]
  "transforms the whole game state in 4 steps:
    1. move the current falling shape
    2. update the game-map with the updated shape
    3. update the game-map with the existing shapes
    4. update the existing shapes vector if the current shape has reach the bottom"
  (let [[shape-updated reached-bottom? old-shape] (move-shape curr-shape dir existing-shapes)
        updated-with-curr-shape (update-game-map-with-shape game-map shape-updated reached-bottom?)
        updated-with-everything (update-game-map-with-existing-shapes updated-with-curr-shape existing-shapes)]
    (assoc state
      :game-map updated-with-everything
      :curr-shape shape-updated
      :existing-shapes (update-existing-shapes-if-needed existing-shapes reached-bottom? old-shape))))

(defn update-state-after-event [state last-event]
  "basically the event dispatcher"
  (case (get last-event 0)
    :left-key {:should-rerender? true, :new-state (transform-state state :left)}
    :right-key {:should-rerender? true, :new-state (transform-state state :right)}
    :rotate-key {:should-rerender? true, :new-state (transform-state state :rotate)}
    :down-key {:should-rerender? true, :new-state (transform-state state :down)}
    :tick {:should-rerender? true, :new-state (transform-state state :down)}
    :nothing {:should-rerender? false, :new-state state}
    :default {:should-rerender? false, :new-state state}))

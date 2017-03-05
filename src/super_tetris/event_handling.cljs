(ns super-tetris.event-handling
  (:use [super-tetris.shape :only [create-shape will-stop? get-left-side-x
                                   get-right-side-x get-bottom-y get-top-y
                                   will-touch-existing-shapes? reached-bottom?
                                   move-to-center move-up move-down
                                   move-left move-right rotate outside-the-map?]])
  (:require [clojure.walk :refer [prewalk]]))

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

(defn update-game-map-with-shape [game-map {:keys [squares]} reached-bottom?]
  (assert squares ":squares is not defined")
  (let [game-map (if-not reached-bottom?
                   (clear-map game-map)
                   game-map)]
    (apply-shape game-map squares)))

(defn update-game-map-with-existing-shapes [game-map existing-shapes]
  (apply-shape game-map existing-shapes))

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
            [(move-up (move-to-center (create-shape))) true shape]
            [(move-down shape) false])
    :rotate [(if (or (outside-the-map? (rotate shape))
                     (will-touch-existing-shapes? (rotate shape) existing-shapes))
               shape
               (rotate shape)) false]
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

(ns super-tetris.event-handling
  (:use [super-tetris.shape :only [create-shape will-stop? get-left-side-x
                                   get-right-side-x get-bottom-y get-top-y
                                   will-touch-existing-shapes? reached-bottom?
                                   move-to-center move-up move-down
                                   move-left move-right rotate outside-the-map?]])
  (:require [clojure.walk :refer [prewalk]]
            [super-tetris.config :refer [horizontal-count vertical-count]]))

(defn apply-shape [game-map shape]
  (letfn [(-show-square [game-map curr]
            (if (neg? (curr 1))
              game-map
              (update-in game-map (conj curr :shown) (constantly true))))]
    (reduce -show-square game-map shape)))

(defn clear-map [game-map]
  (prewalk #(if (true? (:shown %))
             (assoc % :shown false)
             %) game-map))

(defn get-full-rows [existing-shapes {:keys [squares]} ]
  (let [all-squares (vec (concat squares existing-shapes))
        grouped-by-row (group-by second all-squares)]
    (filter (fn [[_ val]] (= (count val) horizontal-count)) grouped-by-row)))

(defn- belongs-to-row? [full-rows one-square]
  (some #{(second one-square)} full-rows))

(defn remove-full-row-squares [squares full-rows]
  (let [filtered-squares (filter (complement (partial belongs-to-row? full-rows)) squares)]
    (map #(if (< (second %) (first full-rows))
           (assoc % 1 (+ (second %) (count full-rows)))
           %) filtered-squares)))

(defn mixin-old-shape [existing-shapes old-shape]
  (vec (concat existing-shapes old-shape)))

(defn update-game-map [game-map {:keys [squares] :as curr-shape} reached-bottom? existing-shapes old-shape square-count]
  (assert squares ":squares is not defined")
  (if reached-bottom?
    (let [full-rows (get-full-rows existing-shapes old-shape)
          updated-existing-shapes (remove-full-row-squares existing-shapes (keys full-rows))
          updated-old-shape (remove-full-row-squares (:squares old-shape) (keys full-rows))
          all-squares (vec (concat updated-old-shape updated-existing-shapes))
          new-square-count (+ square-count (count (keys full-rows)))
          new-shape (if (> (count (keys full-rows)) 0)
                      (move-up (move-to-center (create-shape new-square-count)))
                      curr-shape)]
      #_(prn squares)
      [(apply-shape game-map all-squares) (mixin-old-shape updated-existing-shapes updated-old-shape) new-square-count new-shape])
    (let [all-squares (vec (concat squares existing-shapes))]
      [(apply-shape (clear-map game-map) all-squares) existing-shapes square-count])))

(defn move-shape [shape dir existing-shapes square-count]
  "event dispatcher for moving the shape"
  (case dir
    :left [(if (or (zero? (get-left-side-x shape))
                   (will-touch-existing-shapes? (move-left shape) existing-shapes))
             shape
             (move-left shape)) false]
    :right [(if (or (= (dec horizontal-count) (get-right-side-x shape))
                    (will-touch-existing-shapes? (move-right shape) existing-shapes))
              shape
              (move-right shape)) false]
    :down (if (will-stop? shape existing-shapes)
            [(move-up (move-to-center (create-shape square-count))) true shape]
            [(move-down shape) false])
    :rotate [(if (or (outside-the-map? (rotate shape))
                     (will-touch-existing-shapes? (rotate shape) existing-shapes))
               shape
               (rotate shape)) false]
    :default [shape false]))

(defn transform-state [{:keys [game-map curr-shape existing-shapes square-count] :as state} dir]
  "transforms the whole game state in 4 steps:
    1. move the current falling shape
    2. update the game-map with the updated shape
    3. update the game-map with the existing shapes
    4. update the existing shapes vector if the current shape has reach the bottom"
  (let [[shape-updated reached-bottom? old-shape] (move-shape
                                                    curr-shape
                                                    dir
                                                    existing-shapes
                                                    square-count
                                                    )
        [updated-game-map updated-existing-shapes new-square-count shape-updated-maybe] (update-game-map
                                                                      game-map
                                                                      shape-updated
                                                                      reached-bottom?
                                                                      existing-shapes
                                                                      old-shape
                                                                      square-count)
        shape-updated (if (= new-square-count square-count)
                        shape-updated
                        shape-updated-maybe)]
    #_(prn "square count: " square-count "new square count: " new-square-count)
    (assoc state
      :game-map updated-game-map
      :curr-shape shape-updated
      :existing-shapes updated-existing-shapes
      :square-count new-square-count)))

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

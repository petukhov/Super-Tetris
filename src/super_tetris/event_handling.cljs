(ns super-tetris.event-handling
  (:use [super-tetris.shape :only [create-shape will-stop? get-left-side-x
                                   get-right-side-x get-bottom-y get-top-y
                                   will-touch-existing-shapes? reached-bottom?
                                   move-to-center move-up move-down
                                   move-left move-right rotate outside-the-map?]])
  (:require [clojure.walk :refer [prewalk]]))

(def map-width 10)

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
  (prn existing-shapes squares)
  (let [all-squares (vec (concat squares existing-shapes))
        grouped-by-row (group-by second all-squares)]
    (prn grouped-by-row)
    (filter (fn [[_ val]] (= (count val) map-width)) grouped-by-row)))

(defn clear-rows [existing-shapes full-rows]
  existing-shapes)

(defn clear-rows-in-old-shape [squares full-rows]
  squares)

(defn mixin-old-shape [existing-shapes old-shape]
  (vec (concat existing-shapes (:squares old-shape))))

(defn update-game-map [game-map {:keys [squares]} reached-bottom? existing-shapes old-shape]
  (assert squares ":squares is not defined")
  (if reached-bottom?
    (let [full-rows (get-full-rows existing-shapes old-shape)
          updated-existing-shapes (clear-rows existing-shapes (keys full-rows))
          updated-old-shape (clear-rows-in-old-shape squares (keys full-rows))
          all-squares (vec (concat updated-old-shape updated-existing-shapes))]
      #_(if (not-empty full-rows) (prn "there is a full row!") nil)
      [(apply-shape game-map all-squares) (mixin-old-shape updated-existing-shapes old-shape)])
    (let [all-squares (vec (concat squares existing-shapes))]
      [(apply-shape (clear-map game-map) all-squares) existing-shapes]))

  #_(let [[game-map updated-existing-shapes] (condp = reached-bottom?
                   true (let [full-rows (get-full-rows existing-shapes old-shape)]
                          (prn "full rows" full-rows)
                          (clear-rows game-map existing-shapes (keys full-rows)))
                   false [(clear-map game-map) existing-shapes])
        updated-old-shape (clear-rows-in-old-shape squares full-rows)
        all-squares (vec (concat updated-old-shape updated-existing-shapes))] ; consider adding remove-full rows here. it should take full-rows parameter.
    ;; add another form  in the above let form where full-rows will be found.
    [(apply-shape game-map all-squares) updated-existing-shapes]))

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
        [updated-game-map existing-shapes] (update-game-map game-map shape-updated reached-bottom? existing-shapes old-shape)]
    (assoc state
      :game-map updated-game-map
      :curr-shape shape-updated
      :existing-shapes existing-shapes #_(update-existing-shapes-if-needed existing-shapes reached-bottom? old-shape))))

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

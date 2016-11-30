(ns super-tetris.event-handling
  (:require [clojure.walk :refer [prewalk]]))

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

(defn update-game-map-with-shape [game-map shape]
  (-> game-map
       (clear-map)
       (apply-shape shape)))

(defn update-game-map-with-existing-shapes [game-map existing-shapes]
  (reduce apply-shape game-map existing-shapes))

(defn get-left-side-x [shape]
  (first (first shape)))

(defn get-right-side-x [shape]
  (first (last shape)))

(defn get-bottom-y [shape]
  (second (apply max-key second shape)))

(defn reached-bottom? [shape] (= bottom-y (get-bottom-y shape)))

(defn will-reach-bottom? [shape] (= (dec bottom-y) (get-bottom-y shape)))

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

(defn move-shape [shape dir]
  (case dir
    :left [(if (zero? (get-left-side-x shape))
             shape
             (move-left shape)) false]
    :right [(if (= 9 (get-right-side-x shape))              ; value 9 should be depending on the horizontal count constant.
              shape
              (move-right shape)) false]
    :down (if (reached-bottom? shape)
            [(move-up (move-to-center (make-new-shape))) false]
            [(move-down shape) (will-reach-bottom? shape)])
    :default [shape false]))

(defn update-existing-shapes-if-needed [existing-shapes reached-bottom? shape]
  (if reached-bottom?
    (conj existing-shapes (move-down shape))
    existing-shapes))

(defn move [{:keys [game-map curr-shape existing-shapes] :as state} dir]
  (let [[shape-updated reached-bottom?] (move-shape curr-shape dir)
        updated-with-curr-shape (update-game-map-with-shape game-map shape-updated)
        updated-with-everything (update-game-map-with-existing-shapes updated-with-curr-shape existing-shapes)]
    (assoc state
      :game-map updated-with-everything
      :curr-shape shape-updated
      :existing-shapes (update-existing-shapes-if-needed existing-shapes reached-bottom? curr-shape))))

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :left-key {:should-update? true, :new-state (move state :left)}
    :right-key {:should-update? true, :new-state (move state :right)}
    :tick {:should-update? true, :new-state (move state :down)}
    :nothing {:should-update? false, :new-state state}
    :default))

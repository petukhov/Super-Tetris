(ns super-tetris.event-handling
  (:require [clojure.walk :refer [prewalk]]))

(defn apply-shape [game-map shape]
  (letfn [(show-square [game-map curr]
            (update-in game-map (conj curr :shown) (constantly true)))]
    (reduce show-square game-map shape)))

(defn clear-map [game-map]
  (prewalk #(if (true? (:shown %))
             (assoc % :shown false)
             %) game-map))

(defn update-game-map-with-shape [game-map shape]
  (-> game-map
       (clear-map)
       (apply-shape shape)))

(defn get-left-side-x [shape]
  (first (first shape)))

(defn get-right-side-x [shape]
  (first (last shape)))

(defn get-bottom-y [shape]
  (second (apply max-key second shape)))

(defn make-new-shape []
  [[0 0] [0 1] [0 2] [1 1]])

(defn move-to-center [shape]
  (let [shape-width (- (get-right-side-x shape) (get-left-side-x shape))
        offset (js/Math.floor (- 5 (/ shape-width 2)))]
    (map #(update % 0 + offset) shape)))

(defn move-shape [shape dir]
  (case dir
    :left (if (zero? (get-left-side-x shape))
            shape
            (map #(update % 0 dec) shape))
    :right (if (= 9 (get-right-side-x shape))               ; value 9 should be depending on the horizontal count constant.
             shape
             (map #(update % 0 inc) shape))
    :down (if (= 9 (get-bottom-y shape))
            (move-to-center (make-new-shape))
            (map #(update % 1 inc) shape))
    :default shape))

(defn move [{:keys [game-map curr-shape y-pos] :as state} dir]
  (let [shape-updated (move-shape curr-shape dir)
        game-map-updated (update-game-map-with-shape game-map shape-updated)]
    (assoc state
      :game-map game-map-updated
      :curr-shape shape-updated)))

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :left-key {:should-update? true, :new-state (move state :left)}
    :right-key {:should-update? true, :new-state (move state :right)}
    :tick {:should-update? true, :new-state (move state :down)}
    :nothing {:should-update? false, :new-state state}
    :default))

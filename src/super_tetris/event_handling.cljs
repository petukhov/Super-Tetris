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

(defn move-shape [shape dir]
  (case dir
    :left (map #(update % 0 dec) shape)
    :right (map #(update % 0 inc) shape)
    :down (map #(update % 1 inc) shape)
    :default shape))

(defn move [{:keys [game-map curr-shape y-pos] :as state} dir]
  (let [shape-updated (move-shape curr-shape dir)
        game-map-updated (update-game-map-with-shape game-map shape-updated)]
    (assoc state
      :game-map game-map-updated
      :curr-shape shape-updated)))

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :keydown {:should-update? true, :new-state (move state :right)}
    :tick {:should-update? true, :new-state (move state :down)}
    :nothing {:should-update? false, :new-state state}
    :default))

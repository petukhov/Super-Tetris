(ns super-tetris.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! >! timeout close! alts!]]))

(enable-console-print!)
(declare .requestAnimationFrame)
(declare gap-width)

(defn create-square [h-pos v-pos sq-size]
  {:x (+ (* h-pos sq-size) (* h-pos gap-width))
   :y (+ (* v-pos sq-size) (* v-pos gap-width))
   :shown true})

(defn gen-game-map [vertical-count horizontal-count sq-size]
  (->> (for [i (range horizontal-count)
             k (range vertical-count)]
         (create-square i k sq-size))
       (partition vertical-count)
       (map #(vec %))
       (vec)))

;;constants
(def gap-width 2)
(def initial-state (gen-game-map 10 10 5))

;;setting up the canvas
(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-fillStyle context) "blue")

;;event listeners
(def events-chan (chan 5))
(events/listen js/document "keydown" #(put! events-chan :keydown))
(.setInterval js/window #(put! events-chan :tick) 300)

(defn draw-square [square]
  (when (get square :shown)
    (.fillRect context (get square :x) (get square :y) 5 5)))

(defn render [state]
  (.clearRect context 0 0 500 500)
  (doall (for [sub-vector state
         square sub-vector]
     (draw-square square))))

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :keydown [true (update-in state [6 6 :shown] not)]
    :tick    [true state]
    :nothing [false state]
    :default))

((fn animation-loop [state]
   (go
     (let [last-event (alts! [events-chan] :default :nothing)
           [should-update? new-state] (update-state-after-event state last-event)]
       (when should-update? (render new-state))
       (.requestAnimationFrame js/window (partial animation-loop new-state)))))
  initial-state)


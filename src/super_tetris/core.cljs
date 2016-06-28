(ns super-tetris.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! >! timeout close! alts!]]))

(enable-console-print!)
(declare .requestAnimationFrame)

;;constants
(def gap-width 2)
(def initial-state  {:x 20 :y 10 :dx 30})

;;setting up the canvas
(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-fillStyle context) "blue")
(.fillRect context 10 10 100 100)

;;event listeners
(def events-chan (chan 5))
(events/listen js/document "keydown" #(put! events-chan :keydown))
(.setInterval js/window #(put! events-chan :tick) 300)

(defn create-square [h-pos v-pos sq-size]
  {:x (+ (* h-pos sq-size) (* h-pos gap-width))
   :y (+ (* v-pos sq-size) (* v-pos gap-width))
   :shown true})

(defn gen-game-map [vertical-count horizontal-count sq-size]
  (->> (for [i (range horizontal-count), k (range vertical-count)]
          (create-square i k sq-size))
       (partition vertical-count)
       (map #(vec %))
       (vec)))

(defn render [state]
  (.clearRect context 0 0 500 500)
  (.fillRect context (get state :x) (get state :y) 100 100)
  (println "render"))

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :keydown [true (update state :dx * -1)]
    :tick    [true (update state :x + (get state :dx))]
    :nothing [false state]
    :default))

((fn animation-loop [state]
   (go
     (let [last-event (alts! [events-chan] :default :nothing)
           [should-update? new-state] (update-state-after-event state last-event)]
       (when should-update? (render new-state))
       (.requestAnimationFrame js/window (partial animation-loop new-state)))))
  initial-state)


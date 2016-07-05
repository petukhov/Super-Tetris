(ns super-tetris.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! >! timeout close! alts!]]
            [super-tetris.initialization :refer [init-game-state]]
            [super-tetris.event-handling :refer [update-state-after-event]]))

(enable-console-print!)
(declare .requestAnimationFrame)

;;constants
(def gap-width 2)
(def horizontal-count 10)
(def vertical-count 10)
(def sq-size 15)
(def tick-interval 300)
(def initial-state (init-game-state vertical-count horizontal-count sq-size gap-width))

;;setting up the canvas
(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-fillStyle context) "blue")
(let [{:keys [width height]} initial-state]
  (set! (.-width canvas) width)
  (set! (.-height canvas) height))

;;event listeners
(def events-chan (chan 5))
(events/listen js/document "keydown" #(put! events-chan :keydown))
(.setInterval js/window #(put! events-chan :tick) tick-interval)

;;functions related to rendering
(defn draw-square [square]
  (when (get square :shown)
    (.fillRect context (get square :x) (get square :y) sq-size sq-size)))

(defn render [{:keys [game-map width height]}]
  (.clearRect context 0 0 width height)
  (doall
    (for [sub-vector game-map
          square sub-vector]
      (draw-square square)))
  nil)

((fn animation-loop [state]
   (go
     (let [last-event (alts! [events-chan] :default :nothing)
           [should-update? new-state] (update-state-after-event state last-event)]
       (when should-update? (render new-state))
       (.requestAnimationFrame js/window (partial animation-loop new-state)))))
  initial-state)
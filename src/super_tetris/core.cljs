(ns super-tetris.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! >! timeout close! alts!]]))

(enable-console-print!)

(declare .requestAnimationFrame)

(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-fillStyle context) "blue")
(.fillRect context 10 10 100 100)

(def events-chan (chan 5))
(events/listen js/document "keydown" #(put! events-chan :keydown))
(.setInterval js/window #(put! events-chan :tick) 300)

(def initial-state  {:x 20 :y 10 :dx 30})

(defn gen-game-map [vertical-count horizontal-count sq-height sq-width]
  (let [horizontal-range (range horizontal-count)
        vertical-range (range vertical-count)
        empty-2d-vec (vec (map #(vec (repeat horizontal-count {})) vertical-range))
        indices (for [i horizontal-range
                      j vertical-range]
                  [i j])
        add-fields (fn [acc curr] (assoc-in acc (conj curr :state) :show))]
    (reduce add-fields empty-2d-vec indices)))

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


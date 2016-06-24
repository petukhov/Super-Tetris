(ns super-tetris.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! >! timeout close! alts!]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(declare .requestAnimationFrame)

(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-fillStyle context) "blue")
(.fillRect context 10 10 100 100)

(def events-chan (chan 5))

(defn animation-loop [state]
  (.clearRect context 0 0 500 500)
  (.fillRect context (state :x) (state :y) 100 100)
  (go
    (let [any-event (alts! [events-chan] :default 42)
          updated-dx (if (= (any-event 0) 42)
                       state
                       (update state :dx * -1))
          new-state (update updated-dx :x + (updated-dx :dx))]
      (.requestAnimationFrame js/window (partial animation-loop new-state)))))

(.requestAnimationFrame js/window (partial animation-loop {:x 20 :y 10 :dx 1}))

(events/listen js/document "keydown" #(put! events-chan %))
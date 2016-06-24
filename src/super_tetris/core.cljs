(ns super-tetris.core
  (:require ))

(enable-console-print!)

(println "whatever it's just some text")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(def canvas (.getElementById js/document "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-fillStyle context) "blue")
(.fillRect context 10 10 100 100)


(defn animation-loop [state]
  ;(println "animation is happening" (state :x) (state :y))
  (.clearRect context 0 0 500 500)
  (.fillRect context (state :x) (state :y) 100 100)
  (let [new-state (update state :x + 1)]
    (.requestAnimationFrame js/window (partial animation-loop new-state))))

(.requestAnimationFrame js/window (partial animation-loop {:x 20 :y 10}))

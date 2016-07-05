(ns super-tetris.event-handling)

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :keydown [true (update-in state [:game-map 6 6 :shown] not)]
    :tick    [true state]
    :nothing [false state]
    :default))

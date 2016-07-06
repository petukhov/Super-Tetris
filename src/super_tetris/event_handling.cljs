(ns super-tetris.event-handling)

(defn- move-down [state]
  #_(update-in state [:game-map 6 6 :y] inc)
  state)

(defn update-state-after-event [state last-event]
  (case (get last-event 0)
    :keydown {:should-update? true, :new-state (update-in state [:game-map 6 6 :shown] not)}
    :tick    {:should-update? true, :new-state (move-down state)}
    :nothing {:should-update? false, :new-state state}
    :default))

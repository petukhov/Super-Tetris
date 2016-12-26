(ns super-tetris.shape)


(def initial-squares [[0 0] [0 1] [0 2] [1 1]])


;; helper functions. used in find-center and then in implementation IShape protocol

(defn- -get-left-side-x [squares]
  (first (apply min-key first squares)))

(defn- -get-right-side-x [squares]
  (first (apply max-key first squares)))

(defn- -get-bottom-y [squares]
  (second (apply max-key second squares)))

(defn- -get-top-y [squares]
  (second (apply min-key second squares)))



(defn- find-center [shape]
  (assert (and (vector? shape) (seq shape)) "the parameter should be a non-empty vector")
  (let [top-y (-get-top-y shape)
        left-x (-get-left-side-x shape)
        height (- (-get-bottom-y shape) (-get-top-y shape))
        width (- (-get-right-side-x shape) (-get-left-side-x shape))
        x-offset (+ left-x (.round js/Math (quot width 2)))
        y-offset (+ top-y (.round js/Math (quot height 2)))]
    (prn "leftx and topy: " left-x top-y)
    (prn "width and height: " width height)
    (prn "x-offset and y-offset: " x-offset y-offset)
    [x-offset y-offset]))


(defprotocol IShape
  (get-left-side-x [_])
  (get-right-side-x [_])
  (get-bottom-y [_])
  (get-top-y [_]))

(defrecord Shape [squares center]
  IShape
  (get-left-side-x [_] (-get-left-side-x squares))
  (get-right-side-x [_] (-get-right-side-x squares))
  (get-bottom-y [_] (-get-bottom-y squares))
  (get-top-y [_] (-get-top-y squares)))


(defn create-shape []
  (->Shape initial-squares (find-center initial-squares)))
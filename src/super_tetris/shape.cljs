(ns super-tetris.shape
  (:use [super-tetris.config :only [horizontal-count vertical-count]]))

(enable-console-print!)

(def bottom-y (dec vertical-count))

;; helper functions. used in find-center and then in implementation of IShape protocol

(defn- -get-left-side-x [squares]
  (first (apply min-key first squares)))

(defn- -get-right-side-x [squares]
  (first (apply max-key first squares)))

(defn- -get-bottom-y [squares]
  (second (apply max-key second squares)))

(defn- -get-top-y [squares]
  (second (apply min-key second squares)))


;; helper functions related to rotation

(defn- forward-normalizer [x-offset y-offset [x y]]
  [(- x x-offset) (- y y-offset)])

(defn- backward-normalizer [x-offset y-offset [x y]]
  [(+ x x-offset) (+ y y-offset)])

(defn- normalize [squares normalizer [x-offset y-offset]]
  (vec (map (partial normalizer x-offset y-offset) squares)))

(defn- rotate-normalized [squares]
  (map (fn [[x y]] [(- y) x]) squares))


(defn- find-center [shape]
  (assert (and (vector? shape) (seq shape)) "the parameter should be a non-empty vector")
  (let [top-y (-get-top-y shape)
        left-x (-get-left-side-x shape)
        height (- (-get-bottom-y shape) (-get-top-y shape))
        width (- (-get-right-side-x shape) (-get-left-side-x shape))
        x-offset (+ left-x (.round js/Math (quot width 2)))
        y-offset (+ top-y (.round js/Math (quot height 2)))]
    [x-offset y-offset]))

(defn- put-square-or-nil [shape square dir]
  (let [neighbor-square (-> square
                            (update 0 + (get dir 0))
                            (update 1 + (get dir 1)))]
    (if (some #{neighbor-square} shape)
      nil
      neighbor-square)))

(defn- get-free-positions-around [shape square]
  (vec (filter (complement nil?) (-> []
                                 (conj (put-square-or-nil shape square [1 0]))
                                 (conj (put-square-or-nil shape square [-1 0]))
                                 (conj (put-square-or-nil shape square [0 1]))
                                 (conj (put-square-or-nil shape square [0 -1]))))))

(defn- random-square-on-surface [shape surface]
  (let [some-existing-square (get surface (rand-int (count surface)))
        free-positions (get-free-positions-around shape some-existing-square)
        the-square (get free-positions (rand-int (count free-positions)))
        updated-surface (if (nil? the-square)
                          surface
                          (vec (conj
                                 (if (= (count free-positions) 1)
                                   (remove #(= (first free-positions) %) surface)
                                   surface)
                                 the-square)))]
    (if (nil? the-square) (prn "there is a nil square" some-existing-square))
    [updated-surface the-square]))


(defn- generate-shape [size]
  (loop [shape [[0 0]]
         surface [[0 0]]
         counter 0]
    (if (>= counter size)
      shape
      (let [[updated-surface random-square] (random-square-on-surface shape surface)
            updated-shape (if (nil? random-square)
                            shape
                            (vec (conj shape random-square)))]
        (recur updated-shape updated-surface (inc counter))))))

(defprotocol IShape
  (get-left-side-x [_])
  (get-right-side-x [_])
  (get-bottom-y [_])
  (get-top-y [_])

  (will-touch-existing-shapes? [_ existing-shapes])
  (reached-bottom? [this])
  (will-stop? [this existing-shapes])
  (outside-the-map? [this])

  (move-to-center [this])
  (move-up [this])
  (move-down [_])
  (move-left [_])
  (move-right [_])

  (rotate [_]))


(defrecord Shape [squares center]
  IShape

  (get-left-side-x [_] (-get-left-side-x squares))
  (get-right-side-x [_] (-get-right-side-x squares))
  (get-bottom-y [_] (-get-bottom-y squares))
  (get-top-y [_] (-get-top-y squares))

  (will-touch-existing-shapes? [_ existing-shapes]
    (assert existing-shapes "existing-shapes is not defined")
    (not (empty? (for [s1 squares
                       s2 existing-shapes
                       :when (= s1 s2)]
                   s1))))

  (reached-bottom? [this]
    (=  bottom-y (get-bottom-y this)))

  (will-stop? [this existing-shapes]
    (assert existing-shapes "existing-shapes is not defined")
    (or (reached-bottom? this)
        (will-touch-existing-shapes? (move-down this) existing-shapes)))

  (outside-the-map? [this]
    (or (< (get-left-side-x this) 0)
        (> (get-right-side-x this) (dec horizontal-count))
        (> (get-bottom-y this) bottom-y)))

  (move-to-center [this]
    (let [shape-width (- (get-right-side-x this) (get-left-side-x this))
          offset (js/Math.floor (- (/ horizontal-count 2) (/ shape-width 2)))]
      (->Shape
        (map #(update % 0 + offset) squares)
        (update center 0 + offset))))

  (move-up [this]
    (let [shape-height (inc (get-bottom-y this))]
      (->Shape
        (map #(update % 1 - shape-height) squares)
        (update center 1 - shape-height))))

  (move-down [_]
    (->Shape
      (map #(update % 1 inc) squares)
      (update center 1 inc)))

  (move-left [_]
    (->Shape
      (map #(update % 0 dec) squares)
      (update center 0 dec)))

  (move-right [_]
    (->Shape
      (map #(update % 0 inc) squares)
      (update center 0 inc)))

  (rotate [_]
    (let [offsets center
          squares-rotated (-> squares
                              (normalize forward-normalizer offsets)
                              (rotate-normalized)
                              (normalize backward-normalizer offsets))]
      (->Shape squares-rotated center))))


(defn create-shape []
  (let [generated-shape (generate-shape 10)]
    (->Shape generated-shape (find-center generated-shape))))
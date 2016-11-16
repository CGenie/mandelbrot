(ns mandelbrot.coordinates
  (:require [mandelbrot.complex :as c]))

(defn xy->canvas-scaler
  [params]
  (let [canvas (:canvas params)
        scale (:scale params)
        x-min (:x-min scale)
        y-min (:y-min scale)
        scale-width (- (:x-max scale) x-min)
        scale-height (- (:y-max scale) y-min)
        x-prop (/ (:width canvas) scale-width)
        y-prop (/ (:height canvas) scale-height)]

    (fn [[x y]]
      [(* x-prop (- x x-min))
       (* y-prop (- y y-min))])))


(defn canvas->xy-scaler
  [params]
  (let [canvas (:canvas params)
        height (:height canvas)
        scale (:scale params)
        x-min (:x-min scale)
        y-min (:y-min scale)
        scale-width (- (:x-max scale) x-min)
        scale-height (- (:y-max scale) y-min)
        x-prop (/ (:width canvas) scale-width)
        y-prop (/ height scale-height)]

  (fn [[x y]]
    [(+ x-min (/ x x-prop))
     (+ y-min (/ (- height y) x-prop))])))

(defn scale-points
  [params]
  (let [scale (:scale params)
        canvas (:canvas params)
        x-range (range (:x-min scale) (:x-max scale) (:x-step canvas))
        y-range (range (:y-min scale) (:y-max scale) (:y-step canvas))]
    (mapcat (fn [cx]
              (map (fn [cy] (c/complex. cx cy))
                   y-range))
            x-range)))

(defn canvas-points
  [params]
  (let [canvas (:canvas params)
        x-range (range 0 (:width canvas))
        y-range (range 0 (:height canvas))
        scaler (canvas->xy-scaler params)]
    (mapcat (fn [cx]
              (map (fn [cy] [cx cy])
                   y-range))
            x-range)))

(defn canvas->xy-points
  [params]
  (let [points (canvas-points params)
        scaler (canvas->xy-scaler params)]
    (map scaler points)))

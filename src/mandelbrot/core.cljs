(ns mandelbrot.core
  (:require [mandelbrot.complex :as c]
            [mandelbrot.mandelbrot :as m]
            [mandelbrot.coordinates :as coords]))

(enable-console-print!)

(println "This text is printed from src/mandelbrot/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload
(defonce app-state
  (atom {:canvas-id "canvas"
         :scale {
                 :x-min -2.5
                 :x-max 1
                 :y-min -1.5
                 :y-max 1.5
                 }}))

(defn get-canvas
  []
  (let [canvas (js/document.getElementById (:canvas-id @app-state))
        ctx (.getContext canvas "2d")
        scale (:scale @app-state)
        scale-width (- (:x-max scale) (:x-min scale))
        scale-height (- (:y-max scale) (:y-min scale))
        width (.-width canvas)
        height (.-height canvas)
        center-x (js/Math.abs (* (/ (:x-min scale) scale-width) width))
        center-y (js/Math.abs (* (/ (:y-min scale) scale-height) height))
        x-step (/ scale-width width)
        y-step (/ scale-height height)]

    (set! (.-lineWidth ctx) x-step)

    ; see http://www.html5canvastutorials.com/labs/html5-canvas-graphing-an-equation/
    (doto ctx
      (.translate center-x center-y)
      (.scale (/ width scale-width) (- (/ height scale-height))))
    {:canvas canvas
     :ctx ctx
     :width width
     :height height
     :x-step x-step
     :y-step y-step}))

(defonce global-canvas
  (atom (get-canvas)))

(defn scale-rect
  []
  (let [scale (:scale @app-state)]
    [(:x-min scale) (:y-min scale) (:x-max scale) (:y-max scale)]))

(defn canvas-clear
  ([] (canvas-clear @global-canvas))
  ([canvas]
   (let [scale (:scale @app-state)]
     (.clearRect (:ctx canvas) (:x-min scale) (:y-min scale) (:x-max scale) (:y-max scale)))))

(defn canvas-background
  ([color] (canvas-background color @global-canvas))
  ([color canvas]
   (let [scale (:scale @app-state)
         scale-width (- (:x-max scale) (:x-min scale))
         scale-height (- (:y-max scale) (:y-min scale))]
     ;(canvas-clear canvas)

     (set! (.-fillStyle (:ctx canvas)) color)

     (doto (:ctx canvas)
       (.fillRect (:x-min scale) (:y-min scale) scale-width scale-height)))))

(defn draw-background
  []
  (let [canvas @global-canvas
        scale (:scale @app-state)]

    (canvas-background "#CCC" canvas)

    (set! (.-fillStyle (:ctx canvas)) "#000")

    ; BORDER
    (doto (:ctx canvas)
      (.beginPath)
      (.moveTo (:x-min scale) (:y-min scale))
      (.lineTo (:x-max scale) (:y-min scale))
      (.lineTo (:x-max scale) (:y-max scale))
      (.lineTo (:x-min scale) (:y-max scale))
      (.lineTo (:x-min scale) (:y-min scale))
      (.stroke))

    ; AXES
    ; x-axis
    (doto (:ctx canvas)
      (.beginPath)
      (.moveTo (:x-min scale) 0)
      (.lineTo (:x-max scale) 0)
      (.stroke))

    ; y-axis
    (doto (:ctx canvas)
      (.beginPath)
      (.moveTo 0 (:y-min scale))
      (.lineTo 0 (:y-max scale))
      (.stroke))))

(defn draw-mandelbrot-
  []
  (let [canvas (get-canvas)
        params {:canvas canvas :scale (:scale @app-state)}
        canvas-points (coords/canvas-points params)
        ;points (coords/canvas->xy-points params)
        ;mandelbrot-points (map (fn [pt] {:point pt :in-mandelbrot (m/in-mandelbrot? pt)})
        ;                       points)
        ;point-color (fn [point]
        ;              (if (:in-mandelbrot point)
        ;                "#000"
        ;                "#FFF"))
        ;draw-point (fn [point]
        ;             (if (:in-mandelbrot point)
        ;               (println "in mandelbrot" point)
        ;               nil)
        ;             (set! (.-fillStyle (:ctx canvas)) (point-color point))
        ;             (doto (:ctx canvas)
        ;               (.rect (first (:point point)) (second (:point point)) 1 1)
        ;               (.fill)))]
        ;mandelbrot-points (filter m/in-mandelbrot? points)
        scaler (coords/canvas->xy-scaler params)
        mandelbrot-points (filter (comp m/in-mandelbrot? scaler) canvas-points)
        draw-point (fn [point]
                     (set! (.-fillStyle (:ctx canvas)) "#000")
                     (doto (:ctx canvas)
                       (.fillRect (first point) (second point) 1 1)))]
    (doseq [point (take 20 mandelbrot-points)] (draw-point point))
    (println "points" (take 20 mandelbrot-points))))

(defn draw-mandelbrot
  []
  (let [canvas @global-canvas
        scale (:scale @app-state)
        params {:canvas canvas :scale scale}
        canvas-points (coords/scale-points params)
        mandelbrot-points (filter m/in-mandelbrot? canvas-points)
        draw-point (fn [point]
                     (set! (.-fillStyle (:ctx canvas)) "#000")
                     (doto (:ctx canvas)
                       (.fillRect (.-real point)
                                  (.-imag point)
                                  (:x-step canvas)
                                  (:y-step canvas))))]
    (doseq [point mandelbrot-points] (draw-point point))))

(draw-background)
(draw-mandelbrot)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (draw-background)
  (draw-mandelbrot)
)

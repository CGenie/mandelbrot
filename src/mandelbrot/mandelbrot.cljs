(ns mandelbrot.mandelbrot
  (:require [mandelbrot.complex :as c]
            [clojure.spec :as s]))


(def max-iters 20)


(defn m-f
  [c]
  ;{:pre [(s/valid? ::c/complex c)]}
  (fn [z] (c/add (c/mul z z) c)))

(defn iteratee
  ([c] (iteratee c (c/complex. 0 0)))
  ([c start]
   ;{:pre [(s/valid? ::c/complex c)
   ;       (s/valid? ::c/complex start)]}
   (iterate (m-f c) start)))

(defn in-mandelbrot?
  ([c] (in-mandelbrot? c max-iters))
  ([c max-iters]
   ;{:pre [(s/valid? ::c/complex c)
   ;       (s/valid? number? max-iters)]}
   (let [iteratees (take max-iters (iteratee c))
         condition? (fn [z] (< (c/norm2 z) 4))
         good-iterations (take-while condition? iteratees)]
     (and (not (empty? good-iterations))
          (= (count good-iterations) max-iters)))))


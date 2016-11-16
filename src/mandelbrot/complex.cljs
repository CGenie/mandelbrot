(ns mandelbrot.complex
  (:require [clojure.spec :as s]))

(enable-console-print!)

(comment
(s/def ::complex
  (s/and vector?
         #(= (count %) 2)
         (comp number? first)
         (comp number? second)))

; complex numbers are just tuples (x, y)
(defn neg
  [[x y]]
  {:pre [(s/valid? ::complex [x y])]}
  [(- x) (- y)])

(defn add
  [[x y] [u v]]
  {:pre [(s/valid? ::complex [x y])
         (s/valid? ::complex [u v])]}
  [(+ x u) (+ y v)])

(defn sub
  [z w]
  {:pre [(s/valid? ::complex z)
         (s/valid? ::complex w)]}
  (add z (neg w)))

(defn mul
  [[x y] [u v]]
  {:pre [(s/valid? ::complex [x y])
         (s/valid? ::complex [u v])]}
  [(- (* x u) (* y v)) (+ (* x v) (* y u))])

(defn norm2
  [[x y]]
  {:pre [(s/valid? ::complex [x y])]}
  (+ (* x x) (* y y)))
)



(deftype complex [^double real ^double imag])

(defn neg [^complex z]
  (let [x (double (.-real z))
        y (double (.-imag z))]
    (complex. (- x) (- y))))

(defn add [^complex z1 ^complex z2]
  (let [x1 (double (.-real z1))
        y1 (double (.-imag z1))
        x2 (double (.-real z2))
        y2 (double (.-imag z2))]
    (complex. (+ x1 x2) (+ y1 y2))))

(defn sub [^complex z1 ^complex z2]
  (add z1 (neg z2)))

(defn mul [^complex z1 ^complex z2]
  (let [x1 (double (.-real z1))
        y1 (double (.-imag z1))
        x2 (double (.-real z2))
        y2 (double (.-imag z2))]
    (complex. (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2)))))

(defn norm2 [^complex z]
  (let [x (double (.-real z))
        y (double (.-imag z))]
    (+ (* x x) (* y y))))

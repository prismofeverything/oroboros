(ns oroboros.segment
  (:require [incanter.core :as math]))

(def transform-scale 0.1)
(def homeward 0.05)

(def zero (math/matrix [0 0 0]))
(def iii (math/identity-matrix 3))

(defn magnitude [v]
  (math/sqrt (math/sum-of-squares v)))

(defn normalize [v]
  (math/div v (magnitude v)))

(defn segment [c v]
  {:color c :vertex v})

(defn pick-segment-count []
  (* 10 (+ (rand-int 42) 9)))

(defn unitoid [scale]
  (* scale (rand)))
  
(defn orbitoid [scale]
  (* scale (- (* (rand) 2) 1)))
  
(defn random-matrix
  ([f] (random-matrix f 1.0))
  ([f scale] (math/matrix (map f (repeat 9 scale)) 3)))

(defn pick-color
  ([] (pick-color 1.0))
  ([scale] (math/matrix (map unitoid (repeat 3 scale)))))

(defn pick-vertex
  ([] (pick-vertex 1.0))
  ([scale] (math/matrix (map orbitoid (repeat 3 scale)))))

(defn pick-rotation
  ([] (pick-rotation 1.0))
  ([scale] (math/matrix (map orbitoid (repeat 4 scale)))))

(defn pick-color-transform
  ([] (pick-color-transform 1.0))
  ([scale]
     (let [deviant (random-matrix orbitoid transform-scale)]
       (math/plus iii deviant))))

(defn random-segment [& _]
  (segment (pick-color) (pick-vertex)))

(defn shift
  "shifts the last segment(s) off of the seq and adds the given segment(s) at the head"
  [pushing segments]
  (let [severity (count pushing)]
    (concat pushing (take (- (count segments) severity) segments))))

(defn mix
  "mixes in the proportion of each vector a and b by the given blend between 0 and 1"
  [a b blend]
  (math/plus (math/mult a (- 1 blend)) (math/mult b blend)))

(defn advance-segment
  "mix between one segment and the next based on the blend (between 0 and 1)"
  [a b blend]
  (segment 
   (mix (a :color) (b :color) blend)
   (mix (a :vertex) (b :vertex) blend)))

(defn deviate
  "given the epsilon, deviate the current vector in an arbitrary direction"
  [v epsilon]
  (let [vcount (count v)]
    (math/plus v (math/matrix (map orbitoid (repeat vcount epsilon))))))

(defn deviate-segment
  "deviate the given segment by a certain proportion"
  [seg scales transform]
  (segment 
   (math/mult 0.5 (math/plus 1 (math/mmult transform (math/minus (math/mult 2 (seg :color)) 1))))
   (deviate (seg :vertex) (last scales))))

(defn add-segment
  "modify the given segment by the deviant segment"
  [seg deviant]
  (segment 
   (mix (seg :color) (deviant :color) 0.1) ;; (deviant :color) ;; (seg :color) ;; (pick-color) ;; (add (seg :color) (deviant :color))
   (mix (math/plus (seg :vertex) (deviant :vertex)) zero homeward)))

(defn mix-segments
  "mix the color and vertex of the two segments a and b according to the blend"
  [a b blend]
  (segment 
   (normalize (math/plus (math/mult (a :color) (- 1.0 blend)) (math/mult (b :color) blend)))
   (math/plus (math/mult (a :vertex) (- 1.0 blend)) (math/mult (b :vertex) blend))))

(ns oroboros.core
  (:use [penumbra opengl])
  (:use cantor)
  (:use oroboros.debug)
  (:use [clojure.contrib.seq-utils :only [rand-elt]])
  (:require [penumbra.app :as app]
            [overtone.live :as ot]
            [oroboros.sound :as sound]))

(def deviation-scale 0.3)
(def tones-max 5)

(defn segment [c v]
  {:color c :vertex v})

(defn segment-count []
  (* 10 (+ (rand-int 42) 9)))

(defn unitoid [scale]
  (* scale (rand)))
  
(defn orbitoid [scale]
  (* scale (- (* (rand) 2) 1)))
  
(defn pick-color
  ([] (pick-color 1.0))
  ([scale] (apply vec4 (map unitoid (repeat 4 scale)))))

(defn pick-vertex
  ([] (pick-vertex 1.0))
  ([scale] (apply vec3 (map orbitoid (repeat 3 scale)))))

(defn pick-rotation
  ([] (pick-rotation 1.0))
  ([scale] (apply vec4 (map orbitoid (repeat 4 scale)))))

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
  (add (mul a (- 1 blend)) (mul b blend)))

(defn advance-segment
  "mix between one segment and the next based on the blend (between 0 and 1)"
  [a b blend]
  (segment (mix (a :color) (b :color) blend)
           (mix (a :vertex) (b :vertex) blend)))

(defn deviate
  "given the epsilon, deviate the current vector in an arbitrary direction"
  [v epsilon]
  (add v (apply vec3 (map orbitoid (repeat 3 epsilon)))))

(defn deviate-segment
  "modify the given segment by the deviant segment"
  [segment]
  (segment (pick-color) (deviate (segment :vertex) deviation-scale)))

;; (defn deviate-segment
;;   "modify the given segment by the deviant segment"
;;   [segment deviant]
;;   (segment (pick-color) (deviate (segment :vertex) deviation-scale)))

(defn segment-markers [segments trailing]
  {:next-segment (deviate-segment (first segments))
   :leading-segment (first segments)
   :first-segment (first segments)
   :last-segment (last segments)
   :trailing-segment trailing
   :previous-segment trailing})

(defn make-segments
  "build a list of n segments by starting with a random segment,
   then deviating each next from the last a small amount"
  [n]
  (reduce
   (fn [segments _]
     (cons (deviate-segment (first segments))
           segments))
   [(random-segment)]
   (range (dec n))))

(defn random-moob []
  (sound/moob (+ 60 (* (rand) 811))))

(defn add-moob
  "add a random moob and shift one off the end if the moob limit is full"
  [moobs]
  (let [zero (random-moob)
        final (last moobs)]
    (if (< (count moobs) tones-max)
      (cons zero moobs)
      (let [shifted (cons zero (take (dec (count moobs)) moobs))
            _ (ot/kill (last moobs))]
        shifted))))

(defn reset
  "return the application to a baseline state"
  [state]
  (let [segments (make-segments (segment-count))]
    (merge
     state
     {:moobs (add-moob (state :moobs))
      :fullscreen false
      :rotation 0
      :theta (vec4 0 0 0 0)
      :dtheta (pick-rotation 20)
      :level 0
      :threshold (unitoid 0.5)
      :segments segments}
     (segment-markers segments (deviate-segment (last segments))))))

(defn make-title
  "mix the various components of oroboros together to form a modulating chain"
  []
  (let [parts ["or" "ob" "or" "os" "OR" "OB" "OR" "OS"]
        num (+ (rand-int 20) 10)]
    (apply str (map (fn [_] (rand-elt parts)) (range num)))))

(defn find-largest-display-mode
  "run through all of the system display modes and find the one with the greatest area"
  []
  (let [res (fn [mode] (apply * (mode :resolution)))]
    (reduce #(if (> (res %1) (res %2)) %1 %2)
            {:resolution [0 0]}
            (app/display-modes))))

(defn set-largest-display-mode
  "find the largest display mode and set the resolution to that"
  []
  (let [largest (find-largest-display-mode)]
    (app/display-mode! largest)))

(defn init
  "penumbra application initialization"
  [state]
  (app/title! (make-title))
  (app/vsync! true)
  (set-largest-display-mode)
  (reset (merge state {:moobs []})))

(defn reshape [[x y w h] state]
  ;; (viewport 1920 1080)
  ;; (frustum-view 60.0 (/ (double w) h) -10.0 10.0)
  (debug (app/display-modes))
  (ortho-view -2 2 -2 2 -2 2)
  (merge state {:width w :height h}))

(defn mouse-down [[x y] button state]
  (update-in state [:moobs] add-moob))

(defn key-press [key state]
  (cond
   (= key " ") (reset state)
   (= key :escape) (do (app/fullscreen! (not (state :fullscreen)))
                       (update-in state [:fullscreen] not))
   :else state))

(defn update [[dt t] state]
  (let [progress (/ (state :level) (state :threshold))
        mid (merge state
         {:rotation (rem (+ (state :rotation) (* dt 10)) 360)
          :theta (add (state :theta) (mul (state :dtheta) dt))
          :level (+ (state :level) dt)
          :leading-segment (advance-segment (state :first-segment)
                                            (state :next-segment) progress)
          :trailing-segment (advance-segment (state :previous-segment)
                                             (state :last-segment) progress)
          })
        lead (-> mid :leading-segment :vertex)
        head (-> mid :first-segment :vertex)
        trans (merge mid
         {:orientation (normalize (sub lead head))})]
    
    (if (> (trans :level) (trans :threshold))
      (let [segments (shift [(trans :next-segment)] (trans :segments))]
        (merge trans {:level 0 :segments segments}
               (segment-markers segments (trans :last-segment))))
      trans)))

(defn do-segment [segment]
  (apply color (segment :color))
  (apply vertex (segment :vertex)))

(defn display [[dt t] state]
  ;; (apply translate (sub (vec3 0 0 0) ((state :leading-segment) :vertex)))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))
  (apply rotate (state :theta))
  (draw-polygon
   (do-segment (state :trailing-segment))
   (doall (map do-segment (reverse (state :segments))))
   (do-segment (state :leading-segment)))
   ;;(do-segment {:vertex (vec3 0 0 0) :color (vec4 0 0 0 0)})
   ;; (do-segment (state :leading-segment))
   ;; (doall (map do-segment (state :segments)))
   ;; (do-segment (state :trailing-segment)))
  (app/repaint!))

(defn close [state]
  (ot/stop)
  state)

(defn start []
  (app/start
   {:init init
    :reshape reshape
    :mouse-down mouse-down
    :key-press key-press
    :update update
    :display display
    :close close}
   {}))

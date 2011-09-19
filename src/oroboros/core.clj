(ns oroboros.core
  (:use [penumbra opengl])
  (:use cantor)
  (:use oroboros.debug)
  (:use [clojure.contrib.seq-utils :only [rand-elt]])
  (:require [penumbra.app :as app]
            [overtone.live :as ot]
            [oroboros.sound :as sound]))

(def color-scale 0.05)
(def vertex-scale 1.0)
(def deviant-scales [color-scale vertex-scale])
(def closing-factor 0.3)
(def threshold 0.1)
(def tones-max 5)

(defn vecn
  "retrieve the appropriate cantor vec function for the given n"
  [n]
  (cond
   (= 2 n) vec2
   (= 3 n) vec3
   (= 4 n) vec4))

(defn segment [c v]
  {:color c :vertex v})

(defn segment-count []
  (* 4 (+ (rand-int 42) 9)))
;;  (* 1 (+ (rand-int 42) 9)))

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
  (let [vcount (count v)
        vecf (vecn vcount)]
    (add v (apply vecf (map orbitoid (repeat vcount epsilon))))))

(defn deviate-segment
  "deviate the given segment by a certain proportion"
  [seg scales]
  (apply segment (map #(deviate (seg %1) %2) [:color :vertex] scales)))

(defn add-segment
  "modify the given segment by the deviant segment"
  [seg deviant]
  (segment (pick-color) ;; (add (seg :color) (deviant :color))
           (add (seg :vertex) (deviant :vertex))))

(defn mix-segments
  "mix the color and vertex of the two segments a and b according to the blend"
  [a b blend]
  (segment (add (mul (a :color) (- 1.0 blend)) (mul (b :color) blend))
           (add (mul (a :vertex) (- 1.0 blend)) (mul (b :vertex) blend))))

(defn segment-markers [segments trailing deviant]
  (let [next-segment (add-segment (first segments) deviant)
        last-segment (last segments)
        deviant-segment (deviate-segment deviant deviant-scales)
        deviant-force (length (deviant-segment :vertex))
        cycle-vertex (normalize (sub (last-segment :vertex) (next-segment :vertex)))
        cycle-segment (segment (last-segment :color) (mul cycle-vertex deviant-force))
        closing-segment (mix-segments deviant-segment cycle-segment closing-factor)]
    {:next-segment next-segment
     :leading-segment (first segments)
     :first-segment (first segments)
     :last-segment last-segment
     :trailing-segment trailing
     :previous-segment trailing
     :deviant-segment closing-segment}))

(defn make-segments
  "build a list of n segments by starting with a random segment,
   then deviating each next from the last a small amount"
  [n deviant]
  (reduce 
   (fn [[segments dev] _]
     [(cons (add-segment (first segments) dev) segments)
      (deviate-segment dev deviant-scales)])
   [[(random-segment)] deviant]
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
  (let [initial-deviant (apply merge (map #(assoc {} %1 (%2 %3)) [:color :vertex] [pick-color pick-vertex] deviant-scales))
        [segments deviant] (make-segments (segment-count) initial-deviant)]
    (merge
     state
     {:moobs (add-moob (state :moobs))
      :fullscreen false
      :rotation 0
      :theta (vec4 0 0 0 0)
      :dtheta (pick-rotation 20)
      :level 0
      :threshold (unitoid threshold)
      :segments segments}
     (segment-markers segments (add-segment (last segments) deviant) deviant))))

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

(defn orthon
  "set the perspective to a box with all n dimensions"
  [n]
  (ortho-view (- n) n (- n) n (- n) n))

(defn reshape [[x y w h] state]
  ;; (viewport 1920 1080)
  ;; (frustum-view 60.0 (/ (double w) h) -10.0 10.0)
  (orthon 100)
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
               (segment-markers segments (trans :last-segment) (trans :deviant-segment)) deviant-scales))
      trans)))

(defn do-segment [segment]
  (apply color (segment :color))
  (apply vertex (segment :vertex)))

(defn display [[dt t] state]
  ;; (apply rotate (state :theta))
  (apply translate (sub (vec3 0 0 0) ((state :leading-segment) :vertex)))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))
  (draw-triangle-fan
   ;; (do-segment (segment (vec4 0 0 0 0) (vec3 0 0 0)))
   (do-segment (state :trailing-segment))
   (doall (map do-segment (reverse (state :segments))))
   (do-segment (state :leading-segment)))
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

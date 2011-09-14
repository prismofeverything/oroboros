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
  (+ (rand-int 42) 9))

(defn pick-color []
  (apply vec4 (map (fn [_] (rand)) (range 4))))

(defn pick-vertex []
  (apply vec3 (map (fn [_] (- (* (rand) 2) 1)) (range 3))))

(defn random-segment [& _]
  (segment (pick-color) (pick-vertex)))

(defn shift [segment segments]
  (cons segment (take (- (count segments) 1) segments)))

(defn mix [a b z]
  (add (mul a (- 1 z)) (mul b z)))
  ;; (map #(+ (* %1 (- 1 z)) (* %2 z)) a b))

(defn advance-segment [a b z]
  (segment (mix (a :color) (b :color) z) (mix (a :vertex) (b :vertex) z)))

(defn random-deviation [epsilon]
  (* epsilon (- (* (rand) 2) 1)))

(defn deviate [v epsilon]
  (add v (apply vec3 (map (fn [_] (random-deviation epsilon)) (range 3)))))

(defn deviate-segment [s]
  (segment (pick-color) (deviate (s :vertex) deviation-scale)))

(defn segment-markers [segments trailing]
  {:next-segment (deviate-segment (first segments))
   :leading-segment (first segments)
   :first-segment (first segments)
   :last-segment (last segments)
   :trailing-segment trailing
   :previous-segment trailing})

(defn make-segments [n]
  (reduce (fn [segments _] (cons (deviate-segment (first segments)) segments)) [(random-segment)] (range (dec n))))

(defn random-moob []
  (sound/moob (+ 200 (* (rand) 500))))

(defn add-moob [moobs]
  (let [zero (random-moob)
        final (last moobs)]
    (if (< (count moobs) tones-max)
      (cons zero moobs)
      (let [shifted (cons zero (take (dec (count moobs)) moobs))
            _ (ot/kill (last moobs))]
        shifted))))

(defn reset [state]
  (let [segments (make-segments (segment-count))]
    (merge
     state
     {:moobs (add-moob (state :moobs))
      :rotation 0
      :level 0
      :threshold 0.1
      :segments segments}
     (segment-markers segments (deviate-segment (last segments))))))

(defn make-title []
  (let [parts ["or" "ob" "or" "os" "OR" "OB" "OR" "OS"]
        num (+ (rand-int 20) 10)]
    (apply str (map (fn [_] (rand-elt parts)) (range num)))))

(defn init [state]
  (app/title! (make-title))
  (app/vsync! true)
  (reset (merge state {:moobs []})))

(defn reshape [[x y w h] state]
  (frustum-view 60.0 (/ (double w) h) -2.0 2.0)
  ;; (viewport 1920 1080)
  (ortho-view -2 2 -2 2 -2 2)
  (merge state {:width w :height h}))

(defn mouse-down [[x y] button state]
  (update-in state [:moobs] add-moob))

(defn key-press [key state]
  (cond
   (= key " ") (reset state)
   :else state))

(defn update [[dt t] state]
  (let [progress (/ (state :level) (state :threshold))
        mid (merge state
         {:rotation (rem (+ (state :rotation) (* dt 10)) 360)
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
      (let [segments (shift (trans :next-segment) (trans :segments))]
        (merge trans {:level 0 :segments segments}
               (segment-markers segments (trans :last-segment))))
      trans)))

(defn do-segment [segment]
  (apply color (segment :color))
  (apply vertex (segment :vertex)))

(defn display [[dt t] state]
  ;; (apply translate (sub (vec3 0 0 0) ((state :leading-segment) :vertex)))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))
  (draw-triangle-strip
   (do-segment (state :leading-segment))
   (doall
    (map do-segment (state :segments)))
   (do-segment (state :trailing-segment)))
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

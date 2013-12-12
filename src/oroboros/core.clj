(ns oroboros.core
  (:use [penumbra opengl])
  (:use oroboros.debug)
  (:use oroboros.segment)
  (:require [incanter.core :as math]
            [penumbra.app :as app]))
;;            [overtone.live :as ot]
;;            [oroboros.sound :as sound]))

(def app (atom nil))

(def color-scale 1.0)
(def vertex-scale 1.8)
(def deviant-scales [color-scale vertex-scale])
(def closing-factor 0.2)
(def threshold 0.02)
(def tones-max 5)
(def transform-rate 0.1)
(def identity-rate 0.01)
(def rotation-rate 50)

(defn segment-markers
  "compose the structure of notable segments, mainly the head, tail, leading and trailing segments"
  [segments trailing deviant color-transform]
  (let [next-segment (add-segment (first segments) deviant)
        last-segment (last segments)
        deviant-segment (deviate-segment deviant deviant-scales color-transform)
        deviant-force (magnitude (deviant-segment :vertex))
        cycle-vertex (normalize (math/minus (last-segment :vertex) (next-segment :vertex)))
        cycle-segment (segment (last-segment :color) (math/mult cycle-vertex deviant-force))
        closing-segment (mix-segments deviant-segment cycle-segment closing-factor)
        ;; color-force (- 1.5 (length (next-segment :color)))

        deviant-transform (mix (math/plus color-transform (random-matrix orbitoid transform-rate))
                               iii
                               identity-rate)]

        ;; deviant-transform (mix (mix color-transform (pick-color-transform transform-rate) transform-rate)
        ;;                        iii
        ;;                        identity-rate)]

        ;; deviant-transform (math/matrix
        ;;                    (normalize
        ;;                     (math/vectorize
        ;;                      (mix (math/plus
        ;;                            color-transform
        ;;                            ;; (pick-color-transform transform-rate)))) 3)]
        ;;                            (random-matrix orbitoid transform-rate))
        ;;                           iii
        ;;                           identity-rate))) 3)]
    {:next-segment next-segment
     :leading-segment (first segments)
     :first-segment (first segments)
     :last-segment last-segment
     :trailing-segment trailing
     :previous-segment trailing
     :deviant-segment closing-segment
     :color-transform deviant-transform}))

(defn make-segments
  "build a list of n segments by starting with a random segment,
   then deviating each next from the last a small amount"
  [n deviant transform]
  (reduce 
   (fn [[segments dev trans] _]
     [(cons (add-segment (first segments) dev) segments)
      (deviate-segment dev deviant-scales trans)
      trans])
   [[(random-segment)] deviant transform]
   (range (dec n))))

;; (defn random-moob []
;;   (sound/moob (+ 60 (* (rand) 811))))

;; (defn add-moob
;;   "add a random moob and shift one off the end if the moob limit is full"
;;   [moobs]
;;   (let [zero (random-moob)
;;         final (last moobs)]
;;     (if (< (count moobs) tones-max)
;;       (cons zero moobs)
;;       (let [shifted (cons zero (take (dec (count moobs)) moobs))
;;             _ (ot/kill (last moobs))]
;;         shifted))))

(defn reset
  "return the application to a baseline state"
  [state]
  (let [segment-count (pick-segment-count)
        deviant (apply merge
                       (map #(assoc {} %1 (%2 %3))
                            [:color :vertex]
                            [pick-color pick-vertex]
                            deviant-scales))
        color-transform (pick-color-transform transform-rate)
        segments [(random-segment)]]
;;        [segments deviant color-transform] (make-segments segment-count initial-deviant transform)]
    (merge
     state
     {;; :moobs (add-moob (state :moobs))
      :fullscreen false
      :segment-count segment-count
      :rotation 0
      :rotation-axis (normalize (pick-vertex))
      :center-color (pick-color)
      :color-transform color-transform
      :theta (math/matrix [0 0 0 0])
      :dtheta (pick-rotation 90)
      :level 0
      :threshold (unitoid threshold)
      :segments segments}
     (segment-markers segments (add-segment (last segments) deviant) deviant color-transform))))

(defn make-title
  "mix the various components of oroboros together to form a modulating chain"
  []
  (let [parts ["or" "ob" "or" "os" "OR" "OB" "OR" "OS"]
        num (+ (rand-int 20) 10)]
    (apply str (map (fn [_] (rand-nth parts)) (range num)))))

(defn resolution-area
  [mode]
  (apply * (:resolution mode)))

(defn find-largest-display-mode
  "run through all of the system display modes and find the one with the greatest area"
  []
  (or 
   (last (sort-by resolution-area (app/display-modes))) 
   {:resolution [0 0]}))

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
  (doseq [mode (app/display-modes)]
    (println mode))
  (set-largest-display-mode)
  (reset state)) ;; (merge state {:moobs []})))

(defn orthon
  "set the perspective to a box with all n dimensions"
  [n]
  (ortho-view (- n) n (- n) n (- n) n))

(defn reshape 
  [[x y w h] state]
  ;; (viewport 1920 1080)
  ;; (frustum-view 60.0 (/ (double w) h) -10.0 10.0)
  (println x y w h)
  (orthon 20)
  (merge 
   state 
   {:width w 
    :height h}))

(defn mouse-down 
  [[x y] button state]
  state)
  ;; (update-in state [:moobs] add-moob))

(defn toggle-fullscreen!
  [state]
  (app/fullscreen! (not (state :fullscreen)))
  (let [[w h] (app/size)
        state (reshape [0 0 w h] state)]
    (println "fullscreen" w h)
    (update-in state [:fullscreen] not)))

(defn key-press
  [key state]
  (cond
   (= key " ") (reset state)
   (= key :escape) (toggle-fullscreen! state)
   :else state))

(defn update
  [[dt t] state]
  (let [progress (/ (state :level) (state :threshold))
        growing? (< (count (state :segments)) (state :segment-count))
        mid (merge state
         {:rotation (rem (+ (state :rotation) (* dt rotation-rate)) 360)
          :theta (math/plus (state :theta) (math/mult (state :dtheta) dt))
          :level (+ (state :level) dt)
          :leading-segment (advance-segment
                            (state :first-segment)
                            (state :next-segment)
                            progress)
          :trailing-segment (if growing?
                              (state :previous-segment)
                              (advance-segment
                               (state :previous-segment)
                               (state :last-segment)
                               progress))})
        lead (-> mid :leading-segment :vertex)
        head (-> mid :first-segment :vertex)
        orientation (normalize (math/minus lead head))
        trans (merge mid
         {:orientation orientation :normal -1})] ;; normal??
    
    (if (> (trans :level) (trans :threshold))
      (let [segments (if growing?
                       (cons (trans :next-segment) (trans :segments))
                       (shift [(trans :next-segment)] (trans :segments)))]
        (merge trans {:level 0 :segments segments}
               (segment-markers segments
                                (if growing?
                                  (trans :previous-segment)
                                  (trans :last-segment))
                                (trans :deviant-segment)
                                (trans :color-transform))))
      trans)))

(defn do-segment
  [segment]
  (apply color (segment :color))
  (apply vertex (segment :vertex)))

(defn display
  [[dt t] state]
  ;; (apply rotate (state :theta))
  (apply rotate (cons (state :rotation) (state :rotation-axis)))
  ;; (apply translate (math/minus (vec3 0 0 0) ((state :leading-segment) :vertex)))
  ;; (apply rotate (apply vec4 (cons 0 (state :normal))))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))
  (draw-polygon
   (do-segment (segment (state :center-color) zero))
   (do-segment (state :trailing-segment))
   (doall (map do-segment (reverse (state :segments))))
   (do-segment (state :leading-segment)))
  (app/repaint!))

(defn close
  [state]
  ;; (ot/stop)
  state)

(defn start
  []
  (reset! 
   app
   (app/start
    {:init init
     :reshape reshape
     :mouse-down mouse-down
     :key-press key-press
     :update update
     :display display
     :close close}
    {})))

(defn -main
  []
  (start))

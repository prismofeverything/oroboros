(ns oroboros.core
  (:use [penumbra opengl compute]
        [penumbra.opengl core])
  (:use oroboros.debug)
  (:use [clojure.contrib.seq-utils :only [rand-elt]])
  (:require cantor
            [incanter.core :as math]
            [penumbra.app :as app]
            [penumbra.data :as data]
            [penumbra.opengl.shader :as shader]
            [overtone.live :as ot]
            [oroboros.orb :as orb]
            [oroboros.sound :as sound]))

(def color-scale 1.0)
(def vertex-scale 1.0)
(def deviant-scales [color-scale vertex-scale])
(def transform-scale 0.1)
(def closing-factor 0.2)
(def threshold 0.2)
(def tones-max 5)
(def transform-rate 0.1)
(def identity-rate 0.01)
(def rotation-rate 10)
(def wobble-rate 0.1)
(def homeward 0.05)
(def fft-scale (/ 1.0 sound/fft-window))
(def fft-index (range -1 1 (* 2 fft-scale)))
(def bin-window 512)

(def zero (math/matrix [0 0 0]))
(def iii (math/identity-matrix 3))

(defn magnitude [v]
  (math/sqrt (math/sum-of-squares v)))

(defn normalize [v]
  (math/div v (magnitude v)))

(defn segment [c v]
  {:color c :vertex v})

(defn pick-segment-count []
  (* 1 (+ (rand-int 42) 9)))
  ;; (* 10 (+ (rand-int 42) 9)))

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
  (segment (mix (a :color) (b :color) blend)
           (mix (a :vertex) (b :vertex) blend)))

(defn deviate
  "given the epsilon, deviate the current vector in an arbitrary direction"
  [v epsilon]
  (let [vcount (count v)]
    (math/plus v (math/matrix (map orbitoid (repeat vcount epsilon))))))

(defn deviate-segment
  "deviate the given segment by a certain proportion"
  [seg scales transform]
  (segment (math/mult 0.5 (math/plus 1 (math/mmult transform (math/minus (math/mult 2 (seg :color)) 1))))
           (deviate (seg :vertex) (last scales))))

(defn add-segment
  "modify the given segment by the deviant segment"
  [seg deviant]
  (segment (mix (seg :color) (deviant :color) 0.1) ;; (deviant :color) ;; (seg :color) ;; (pick-color) ;; (add (seg :color) (deviant :color))
           (mix (math/plus (seg :vertex) (deviant :vertex)) zero homeward)))

(defn mix-segments
  "mix the color and vertex of the two segments a and b according to the blend"
  [a b blend]
  (segment (normalize (math/plus (math/mult (a :color) (- 1.0 blend)) (math/mult (b :color) blend)))
           (math/plus (math/mult (a :vertex) (- 1.0 blend)) (math/mult (b :vertex) blend))))

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

(defn random-moob []
  (sound/moob (+ 60 (* (rand) 811))))

(defn add-moob
  "add a random moob and shift one off the end if the moob limit is full"
  [moobs])
  ;; (let [zero (random-moob)
  ;;       final (last moobs)]
  ;;   (if (< (count moobs) tones-max)
  ;;     (cons zero moobs)
  ;;     (let [shifted (cons zero (take (dec (count moobs)) moobs))
  ;;           _ (ot/kill (last moobs))]
  ;;       shifted))))

(defn make-shader []
  (shader/compile-source

   :vertex
   "
   varying vec4 color;

   void main()
   {
     color = gl_Color;
     gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   } "

   :fragment
   "
   uniform vec2 dim;

   uniform float lo;
   uniform float mid;
   uniform float hi;

   uniform vec3 lopos;
   uniform vec3 midpos;
   uniform vec3 hipos;

   uniform vec3 locolor;
   uniform vec3 midcolor;
   uniform vec3 hicolor;

   void main()
   {
     vec3 frag = vec3(gl_FragCoord.xy, 0);
     vec3 rel = vec3((frag.x / dim.y) - 0.5, ((dim.y - frag.y) / dim.y) - 0.5, 0) * 2.0;
     float loradius = 0.4 * sqrt((210.0 / lo) * dot(rel - lopos, rel - lopos));
     float midradius = 0.4 * sqrt((210.0 / mid) * dot(rel - midpos, rel - midpos));
     float hiradius = 0.4 * sqrt((210.0 / hi) * dot(rel - hipos, rel - hipos));
     float portion = loradius + midradius + hiradius;
     float loportion = loradius / portion;
     float midportion = midradius / portion;
     float hiportion = hiradius / portion;
     float intensity = 1.0 / loradius + 1.0 / midradius + 1.0 / hiradius;
     intensity = clamp(intensity, 0.0, 0.9);
     vec3 blend = (locolor * loportion + midcolor * midportion + hicolor * hiportion) * intensity;

     gl_FragColor = vec4(blend, 1);
   } "

   ))

   ;; uniform vec3 locolor;
   ;; uniform vec3 midcolor;
   ;; uniform vec3 hicolor;


(defn merge-ranges [s win]
  (let [pos (map math/abs s)
        lows (take 16 (drop 2 pos))
        mids (take 68 (drop 18 pos))
        highs (take 426 (drop 86 pos))
        ;; lows (take 10 (drop 2 pos))
        ;; mids (take 150 (drop 12 pos))
        ;; highs (take 350 (drop 162 pos))
        sums (map #(* %2 (apply + %1)) [lows mids highs] [0.01 0.01 0.01])]
    (doall sums)))

(defn orbiting [a b]
  
  (fn [orb]
    (assoc orb :position (orb :position))))

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
        frequencies (ot/buffer-data sound/fft-in)
        [l m h] (debug (merge-ranges frequencies bin-window))
        lo  (orb/make-orb l [0 0 0] [0.1 0.5 0.9] (fn [orb] orb))
        mid (orb/make-orb m [0.5 0.5 0] [0.9 0.1 0.5] (fn [orb] orb))
        hi  (orb/make-orb h [1 1 0] [0.5 0.9 0.1] (fn [orb] orb))
        segments [(random-segment)]]
;;        [segments deviant color-transform] (make-segments segment-count initial-deviant transform)]
    (merge
     state

     {:moobs [];;(add-moob (state :moobs))
      :fullscreen false
      :segment-count segment-count
      :rotation 0
      :rotation-axis (normalize (pick-vertex))
      :center-color (pick-color)
      :color-transform color-transform
      :theta (math/matrix [0 0 0 0])
      :dtheta (pick-rotation 90)
      :level 0
      :shader (make-shader)
      :orbs [lo mid hi]
      :threshold threshold ;; (unitoid threshold)
      :segments segments
      :frequencies frequencies
      :bins [l m h]}

     (segment-markers segments (add-segment (last segments) deviant) deviant color-transform))))

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
  (sound/input-frequencies)
  (debug (shaders-supported?))
  (reset (merge state {:moobs []})))

(defn orthon
  "set the perspective to a box with all n dimensions"
  [n]
  (ortho-view (- n) n (- n) n (- n) n))

(defn reshape [[x y w h] state]
  ;; (viewport 1920 1080)
  ;; (frustum-view 60.0 (/ (double w) h) -10.0 10.0)
  (orthon 1)
  (merge state {:width w :height h}))

 (defn mouse-down [[x y] button state] state)
;;   (update-in state [:moobs] add-moob))

(defn key-press [key state]
  (cond
   (= key " ") (reset state)
   (= key :escape) (do (app/fullscreen! (not (state :fullscreen)))
                       (update-in state [:fullscreen] not))
   :else state))

(defn update [[dt t] state]
  (let [progress (/ (state :level) (state :threshold))
        growing? (< (count (state :segments)) (state :segment-count))
        frequencies (ot/buffer-data sound/fft-in)
        bins (merge-ranges frequencies bin-window)
        leading (advance-segment
                 (state :first-segment)
                 (state :next-segment)
                 progress)
        trailing (if growing?
                   (state :previous-segment)
                   (advance-segment
                    (state :previous-segment)
                    (state :last-segment)
                    progress))
        midpos (map * (leading :vertex) [0.1 0.1 0.01])
        hipos (map * (trailing :vertex) [0.1 0.1 0.01])
        lopos (map * midpos hipos)
        straight (/ 0.02 (reduce + 0 bins))
        inverse (* 0.3 (reduce + 0 bins))
        wobble (* 0.5 (+ 1 (math/sin (* t wobble-rate))))
        threshold (mix straight inverse wobble)
        mid (merge state
         {:rotation (rem (+ (state :rotation) (* dt rotation-rate)) 360)
          :theta (math/plus (state :theta) (math/mult (state :dtheta) dt))
          :level (+ (state :level) dt)
          :frequencies frequencies
          :lopos lopos
          :midpos midpos
          :hipos hipos
          :bins bins
          :threshold threshold
          :orbs (map #(orb/advance-orb (orb/update-orb %1 %2)) (state :orbs) bins)
          :leading-segment leading 
          :trailing-segment trailing})
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

(defn do-segment [segment]
  (apply color (segment :color))
  (apply vertex (segment :vertex)))

(defn display [[dt t] state]
  ;; (apply rotate (cons (state :rotation) (state :rotation-axis)))

  ;; (apply rotate (state :theta))
  ;; (apply translate (math/minus (math/matrix [0 0 0]) ((state :leading-segment) :vertex)))
  ;; (apply rotate (apply vec4 (cons 0 (state :normal))))
  ;; (apply rotate (cons (state :rotation) (state :orientation)))

  (let [[lo mid hi] (state :orbs)
        [w h] (app/size)]
    (with-program (state :shader)
      (uniform :dim (float w) (float h))
      (uniform :timeline t)
      (uniform :lo (lo :magnitude))
      (uniform :mid (mid :magnitude))
      (uniform :hi (hi :magnitude))
      (apply uniform (cons :lopos (state :lopos)))
      (apply uniform (cons :midpos (state :midpos)))
      (apply uniform (cons :hipos (state :hipos)))
      ;; (apply uniform (cons :lopos (math/to-list (lo :position))))
      ;; (apply uniform (cons :midpos (math/to-list (mid :position))))
      ;; (apply uniform (cons :hipos (math/to-list (hi :position))))
      (apply uniform (cons :locolor (math/to-list (lo :color))))
      (apply uniform (cons :midcolor (math/to-list (mid :color))))
      (apply uniform (cons :hicolor (math/to-list (hi :color))))
      (draw-quads
       (vertex -1 -1 0)
       (vertex 1 -1 0)
       (vertex 1 1 0)
       (vertex -1 1 0))))

      ;; (draw-quads
      ;;  (vertex -30 -30 0)
      ;;  (vertex -30 30 0)
      ;;  (vertex 30 30 0)
      ;;  (vertex 30 -30 0))))
      

  ;; (draw-triangle-fan
  ;;  (let [bands (count (state :bins))]
  ;;    (color 0.9 0.5 0.3)
  ;;    (vertex 0 0 0)
  ;;    (doall (map #(vertex %1 (* 10 %2) 0) (range bands) (state :bins)))
  ;;    (vertex (dec bands) 0 0)))

  ;; (draw-triangle-strip
  ;; ;; (draw-polygon
  ;; ;;  (do-segment (segment (state :center-color) zero))
  ;;  (do-segment (state :trailing-segment))
  ;;  (doall (map do-segment (reverse (state :segments))))
  ;;  (do-segment (state :leading-segment)))
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

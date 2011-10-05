(ns oroboros.orb
  (:require [incanter.core :as math]))

(defn make-orb [magnitude position color motion]
  {:magnitude magnitude
   :position (math/matrix position)
   :color (math/matrix color)
   :motion motion})

(defn advance-orb
  "apply the motion function to this orb"
  [orb]
  ((orb :motion) orb))

(defn update-orb
  "notify the orb of its new magnitude"
  [orb mag]
  (assoc orb :magnitude mag))
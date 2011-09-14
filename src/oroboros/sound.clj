(ns oroboros.sound
  (:use overtone.live))

;;(boot :external 57110)

(definst moob [freq 333] (* 0.1 (sin-osc freq)))


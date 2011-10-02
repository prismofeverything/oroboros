(ns oroboros.sound
  (:use overtone.live))

;;(boot :external 57110)

(definst moob [freq 333] (* 0.1 (sin-osc freq)))

(def fft-window 1024)
(def fft-in (buffer fft-window))

(definst input-frequencies []
  (fft fft-in (in 8)))

;; (def a (buffer 2048))
;; (def b (buffer 2048))

;; (demo 10
;;   (let [input (in 8) ; mic
;;         src (pink-noise) ; synth
;;         formed (pv-mul (fft a input) (fft b src))
;;         audio (ifft formed)
;;         reverbed (g-verb audio 9 0.7 0.7)]
;;     (pan2 (* 0.1 reverbed))))
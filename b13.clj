(ns b13 (:use [overtone.live]) (:require [viritystone.tone :as t]) )

(do
  (defn note->hz [music-note]
    (midi->hz (note music-note)))

    (do
    (defonce master-rate-bus (control-bus))
    (defonce root-trg-bus (control-bus)) ;; global metronome pulse
    (defonce root-cnt-bus (control-bus)) ;; global metronome count

    (defonce b1st_beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
    (defonce b1st_beat-cnt-bus (control-bus)) ;; beat count

    (defonce b4th_beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
    (defonce b4th_beat-cnt-bus (control-bus)) ;; beat count

    (defonce b8th_beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
    (defonce b8th_beat-cnt-bus (control-bus)) ;; beat count

    (defonce b16th_beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
    (defonce b16th_beat-cnt-bus (control-bus)) ;; beat count

    (defonce b32th_beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
    (defonce b32th_beat-cnt-bus (control-bus)) ;; beat count

    (def FRACTION_1 "Number of global pulses per beat" 1)
    (def FRACTION_4 "Number of global pulses per beat" 4)
    (def FRACTION_8 "Number of global pulses per beat" 8)
    (def FRACTION_16 "Number of global pulses per beat" 16)
    (def FRACTION_32 "Number of global pulses per beat" 32)

    )

  (do
    (defsynth root-trg [rate 100]
      (out:kr root-trg-bus (impulse:kr (in:kr rate))))

    (defsynth root-cnt []
      (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

    (defsynth b1st_beat-trg [div FRACTION_1]
      (out:kr b1st_beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

    (defsynth b1st_beat-cnt []
      (out:kr b1st_beat-cnt-bus (pulse-count (in:kr b1st_beat-trg-bus))))

    (defsynth b4th_beat-trg [div FRACTION_4]
      (out:kr b4th_beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

    (defsynth b4th_beat-cnt []
      (out:kr b4th_beat-cnt-bus (pulse-count (in:kr b4th_beat-trg-bus))))

    (defsynth b8th_beat-trg [div FRACTION_8]
      (out:kr b8th_beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

    (defsynth b8th_beat-cnt []
      (out:kr b8th_beat-cnt-bus (pulse-count (in:kr b8th_beat-trg-bus))))

    (defsynth b16th_beat-trg [div FRACTION_16]
      (out:kr b16th_beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

    (defsynth b16th_beat-cnt []
      (out:kr b16th_beat-cnt-bus (pulse-count (in:kr b16th_beat-trg-bus))))

    (defsynth b32th_beat-trg [div FRACTION_32]
      (out:kr b32th_beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

    (defsynth b32th_beat-cnt []
      (out:kr b32th_beat-cnt-bus (pulse-count (in:kr b32th_beat-trg-bus))))
    )

  (do
    (def r-trg (root-trg master-rate-bus))
    (def r-cnt (root-cnt [:after r-trg]))
    (def b1st-trg (b1st_beat-trg [:after r-trg]))
    (def b1st-cnt (b1st_beat-cnt [:after b1st-trg]))
    (def b4th-trg (b4th_beat-trg [:after r-trg]))
    (def b4th-cnt (b4th_beat-cnt [:after b4th-trg]))
    (def b8th-trg (b8th_beat-trg [:after r-trg]))
    (def b8th-cnt (b8th_beat-cnt [:after b8th-trg]))
    (def b16th-trg (b16th_beat-trg [:after r-trg]))
    (def b16th-cnt (b16th_beat-cnt [:after b16th-trg]))
    (def b32th-trg (b32th_beat-trg [:after r-trg]))
    (def b32th-cnt (b32th_beat-cnt [:after b32th-trg]))
    (control-bus-set! master-rate-bus (* 2 36))
    )
  (do
    (defonce main-g (group "main bus"))
    (defonce early-g (group "early bus" :head main-g))
    (defonce later-g (group "late bus" :after early-g)))


  (do
    (defonce mcbus1 (control-bus 10))
    )

  (do
    (defonce buffer-64-1 (buffer 64))
    (defonce buffer-64-2 (buffer 64 10))
    )

  (do (defonce cbus1 (control-bus 1)))

  )

                                        ;buffers
(buffer-write! buffer-64-1 [ 2 0 2 0 2 0 2 0
                             2 0 0 0 0 0 0 0
                             2 0 0 0 0 0 0 0
                             2 0 0 0 2 0 0 0
                             2 0 0 0 0 0 0 0
                             2 0 0 0 0 0 0 0
                             2 0 0 0 0 0 0 0
                             2 0 0 0 0 0 0 0])

(buffer-get buffer-64-1 63)



                                        ; single pulse point
; p=[gate, f1, f2, f3, f4, a, d, s, r, amp]

                                        ;Collection of points
(def pointBuffer (buffer (* 5 10)))
(buffer-write! pointBuffer [0 0 0 0 0 0 0 0 0 0
                            1 100 400 30 1 1 1 1 1 1
                            1 200 300 30 1 1 1 1 1 1
                            1 300 200 30 1 1 1 1 1 1
                            1 400 100 30 1 1 1 1 1 1])


(def playBuffer (buffer 32))
(buffer-write! playBuffer [1 1 1 1 3 3 3 3
                           4 4 4 4 3 3 3 3
                           2 2 2 2 2 2 2 2
                           0 0 0 0 2 2 2 2 ])


                                        ;readers


(defn makeBuffer [{:keys [input] :as all}] (let [length        (count (:gate all))
                                                 channels      (count all)
                                                 gate          (:gate all)
                                                 freq          (:freq all)
                                                 a             (:a all)
                                                 d             (:d all)
                                                 s             (:s all)
                                                 r             (:r all)
                                                 buff (buffer  length channels)]
                                             (buffer-write! buff (* 0 length) gate)
                                             ;(buffer-set! buff   (* 1 length)  0)
                                             ;(buffer-write! buff (* 2 length) a)
                                             ;(buffer-write! buff (* 3 length) d)
                                             ;(buffer-write! buff (* 4 length) s)
                                             ;(buffer-write! buff (* 5 length) r)
                                             buff
                                             ))

(defsynth playReader [play-buf 0
                      point-buf 0
                      in-bus-ctr 0
                      outbus 0 ]
    (let [ctr-in    (in:kr in-bus-ctr)
          point     (buf-rd:kr 1 play-buf ctr-in)
          values    10
          maxp      5
          gate      (buf-rd:kr 1 point-buf (+ 0 (* values point)))
          f1        (buf-rd:kr 1 point-buf (+ 1 (* values point)))
          f2        (buf-rd:kr 1 point-buf (+ 2 (* values point)))
          f3        (buf-rd:kr 1 point-buf (+ 3 (* values point)))
          f4        (buf-rd:kr 1 point-buf (+ 4 (* values point)))
          a         (buf-rd:kr 1 point-buf (+ 5 (* values point)))
          d         (buf-rd:kr 1 point-buf (+ 6 (* values point)))
          s         (buf-rd:kr 1 point-buf (+ 7 (* values point)))
          r         (buf-rd:kr 1 point-buf (+ 8 (* values point)))
          amp       (buf-rd:kr 1 point-buf (+ 9 (* values point)))
          ]
      (out:kr outbus [gate f1 f2 f3 f4 a d s r amp])))

(def br (playReader :play-buf playBuffer :point-buf pointBuffer :in-bus-ctr b4th_beat-cnt-bus :outbus mcbus1))

(ctl br :beat-buf (makeBuffer generalInput))

(kill br)

(control-bus-get mcbus1)

(defonce lp (atom true))
(defonce b1stcntr (atom 0))
(reset! lp true)

(def monitor-loop (future (while @lp (do (reset! b1stcntr (control-bus-get b4th_beat-cnt-bus)) (Thread/sleep 1)))))

(add-watch b1stcntr :b1 (fn [_ _ old new] (let [;cnt (int (first (take-last 1 new)))
                                               cnt (int (first (take 1 new)))
                                               gate (int (buffer-get buffer-64-1 (mod cnt 64)))
                                               freq 100]
                                           (control-bus-set! mcbus1 gate 0)
                                           (println "new" cnt gate)
                                           )))

(stop)
                                        ;Synths
  (defsynth mcsynth
    [control-bus 0
     amp  0.3
     osc1 1
     osc2 1
     osc3 1
     osc1-level 1
     osc2-level 1
     osc3-level 1
     cutoff 500
     attack 0.001
     decay 0.3
     sustain 0.99
     release 0.01
     fattack 0.001
     fdecay 0.3
     fsustain 0.99
     frelease 0.01
     sl1 0
     sl2 1
     sl3 2
     ctrl-output 0]
    (let [control_in   (in:kr control-bus 10)
          freq1 (select:kr 1 control_in)
          freq2 (select:kr 2 control_in)
          freq3 (select:kr 3 control_in)
          gate       (select:kr 0 control_in)
          adj        (max 1 gate)
          osc-bank-1 [(saw freq1) (sin-osc freq1) (pulse freq1)]
          osc-bank-2 [(saw freq2) (sin-osc freq2) (pulse freq2)]
          osc-bank-3 [(saw freq3) (sin-osc freq3) (pulse freq3)]
          amp-env    (env-gen (adsr attack decay sustain release) :gate gate)
          ctrl-amp-env    (env-gen:kr (adsr attack decay sustain release) :gate gate)
          f-env      (env-gen (adsr fattack fdecay fsustain frelease) :gate gate)
          ctrl-f-env (a2k amp-env)
          _          (out:kr ctrl-output (* ctrl-f-env))
          s1         (* osc1-level (select osc1 osc-bank-1))
          s2         (* osc2-level (select osc2 osc-bank-2))
          s3         (* osc3-level (select osc3 osc-bank-3))
          filt       (moog-ff (+ s1 s2 s3) (* cutoff f-env) 3)]
      (out 0 (pan2 (* amp amp-env filt)))))

(do (kill mcs1)
    (def mcs1 (mcsynth :control-bus mcbus1))

    )

(ctl mcs1 :amp 4)

(control-bus-set! mcbus1 0 0)

(kill 6240)

(pp-node-tree)

(stop)

(defsynth aaa [] (out 0 (sin-osc 100)))

(def aaaa (aaa))

(kill mcs1)

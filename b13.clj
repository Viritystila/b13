(ns b13 (:use [overtone.live]) (:require [viritystone.tone :as t]) )


(do
  (do
    (defn note->hz [music-note]
      (midi->hz (note music-note)))

    (defn writeBuffer [buffer value-array] (buffer-write! buffer value-array) value-array)

    (defn setChords [value-array note chordval point] (let [values          10
                                                            chrd            (chord note chordval)
                                                            freqs           (vec (map  note->hz chrd))
                                                            maxChordLength  (min (count freqs) 4)
                                                            freqs           (into [] (subvec freqs 0 maxChordLength))
                                                            base-indices    (range (count freqs))
                                                            base-indices    (map + base-indices (repeat maxChordLength (+ 1 (* values point))))]
                                                        (apply assoc value-array (interleave base-indices  freqs ))))

     (defn setADSR [value-array a d s r point] (let [values          10
                                                     adsr            [a d s r]
                                                     base-indices    (range 4)
                                                     base-indices    (map + base-indices (repeat 4 (+ 5 (* values point))))]
                                                 (apply assoc value-array (interleave base-indices  adsr ))))


     (defn setAmp [value-array amp point] (let [values          10 ]
                                            (assoc value-array (+ 9 (* values point)) amp)))


     (defn setAmp [value-array amp point] (let [values          10 ]
                                            (assoc value-array (+ 9 (* values point)) amp)))


     (defn makeBuffer [points ] (let [values      10
                                      buff        (buffer (* points values))
                                      value-array (into [] (repeat (* points values) 0 ))]
                                  (buffer-write! buff value-array)
                                  value-array))

    )

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


  (do (defonce pointLength 10))
  )

                                        ;buffers
(buffer-write! buffer-64-1 [ 4 0 3 0 2 0 2 0
                             2 0 0 0 2 0 0 0
                             1 0 3 0 0 0 4 0
                             3 0 0 0 4 0 0 0
                             1 0 0 0 2 0 0 0
                             3 0 1 0 1 0 0 0
                             4 0 0 0 2 2 2 2
                             2 2 2 2 4 3 4 3])


(buffer-write! buffer-64-1 [ 4 4 4 4 4 4 4 4
                             4 4 4 4 4 4 4 4
                             4 4 4 4 4 4 4 4
                             3 3 3 3 3 3 3 3
                             2 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2
                             2 2 2 2 4 3 4 3])


                                        ; single pulse point
; p=[gate, f1, f2, f3, f4, a, d, s, r, amp]
                                        ;Collection of points
(def pointBuffer (buffer (* 5 pointLength)))
(buffer-write! pointBuffer [0 0 0 0 0 0 0 0 0 0
                            1 (nth (map note->hz (chord :C1 :minor)) 0) 40 30 1 0.01 0.3 0.99 0.01 1
                            1 (nth (map note->hz (chord :C2 :minor)) 0) 30 30 1 0.01 0.3 0.99 0.01 1
                            1 (nth (map note->hz (chord :C3 :minor)) 0) 20 30 1 0.01 0.3 0.99 0.01 1
                            1 (nth (map note->hz (chord :C4 :minor)) 0) 10 30 1 0.01 0.3 0.99 0.01 1])


(def playBuffer (buffer 128))
(buffer-write! playBuffer [4 0 4 0 4 0 4 0
                           4 0 4 0 4 0 4 0
                           4 0 4 0 4 0 4 0
                           4 0 4 0 4 0 4 0
                           4 0 4 0 4 0 4 0
                           4 0 4 0 4 0 4 0
                           3 0 3 0 3 0 3 0
                           3 0 3 0 3 0 3 0
                           2 0 2 0 2 0 2 0
                           2 0 2 0 2 0 2 0
                           2 0 2 0 2 0 2 0
                           2 0 2 0 2 0 2 0
                           2 0 2 0 2 0 2 0
                           2 0 2 0 2 0 2 0
                           2 0 2 0 2 0 2 0
                           4 0 3 0 4 0 3 0])


                                        ;buffer modifiers

(def modValArray (writeBuffer pointBuffer  [0 0 0 0 0 0 0 0 0 0
                             1 (nth (map note->hz (chord :E1 :minor)) 0) 40 30 1 0.01 0.3 0.99 0.01 1
                             1 (nth (map note->hz (chord :B2 :minor)) 0) 30 30 1 0.01 0.3 0.99 0.01 1
                             1 (nth (map note->hz (chord :C3 :minor)) 0) 20 30 1 0.01 0.3 0.99 0.01 1
                             1 (nth (map note->hz (chord :D4 :minor)) 0) 10 30 1 0.01 0.3 0.99 0.01 1] ))


(def modValArray (writeBuffer pointBuffer  (setChords modValArray :E#2 :minor 1)))

(def modValArray (writeBuffer pointBuffer (setADSR modValArray 0.01 0.3 0.99 0.1 4)))

(def modValArray (writeBuffer pointBuffer (setAmp modValArray 1 3)))

(def pointbuf2 (makeBuffer 10))

(def modValArray (writeBuffer pointBuffer (setPoint modValArray [1 200 334 455 576 0.01 0.3 0.99 0.01 1] 3)))

                                        ;readers

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

(ctl br :point-buf pointBuffer :play-buf buffer-64-1 :in-bus-ctr b4th_beat-cnt-bus)

(ctl br :point-buf pointBuffer :play-buf playBuffer)

(kill br)

(control-bus-get mcbus1)

(stop)
                                        ;Synths
;Mcsynth
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
          a     (select:kr 5 control_in)
          d     (select:kr 6 control_in)
          s     (select:kr 7 control_in)
          r     (select:kr 8 control_in)
          gate       (select:kr 0 control_in)
          adj        (max 1 gate)
          osc-bank-1 [(saw freq1) (sin-osc freq1) (pulse freq1)]
          osc-bank-2 [(saw freq2) (sin-osc freq2) (pulse freq2)]
          osc-bank-3 [(saw freq3) (sin-osc freq3) (pulse freq3)]
          amp-env    (env-gen (adsr a d s r) :gate gate)
          f-env      (env-gen (adsr a d s r) :gate gate)
          f-env2     (env-gen:kr (adsr a d s r freq1 :gate gate))
          ctrl-f-env (a2k amp-env)
          _          (out:kr ctrl-output (* ctrl-f-env))
          s1         (* osc1-level (select osc1 osc-bank-1))
          s2         (* osc2-level (select osc2 osc-bank-2))
          s3         (* osc3-level (select osc3 osc-bank-3))
          filt       (moog-ff (+ s1 s2 s3) (* cutoff f-env) 3)
          filt       (free-verb filt f-env2 1 1)
          ]
      (out 0 (pan2 (* amp amp-env filt)))))

(do (kill mcs1)
    (def mcs1 (mcsynth [:tail early-g]  :control-bus mcbus1 :amp 1 :osc1 2 :osc2 0))

    )

(ctl mcs1 :amp 1 :osc1 2 :osc2 0 :cutoff 400)

(kill mcs1)

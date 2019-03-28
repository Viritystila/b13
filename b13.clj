(ns b13 (:use [overtone.live]) (:require [viritystone.tone :as t]) )

;(defsynth aaa []  (out 0 (sin-osc 100)))

;(def aa (aaa))

;(kill aa)

(do
  (do
    (defn note->hz [music-note]
      (midi->hz (note music-note)))

    (defn writeBuffer [buffer value-array] (let [va  (into [] (flatten value-array))]
                                             (buffer-write! buffer va) va))

    (defn setChords [value-array note chordval point] (let [values          10
                                                            chrd            (chord note chordval)
                                                            freqs           (vec (map  note->hz chrd))
                                                            maxChordLength  (min (count freqs) 4)
                                                            freqs           (into [] (subvec freqs 0 maxChordLength))
                                                            base-indices    (range (count freqs))
                                                            base-indices    (into [] (map + base-indices (repeat maxChordLength (+ 1 (* values point)))))]
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
    (defonce mcbus2 (control-bus 10))
    (defonce mcbus3 (control-bus 10))
    )

  (do
    (defonce buffer-64-1 (buffer 64))
    (defonce buffer-64-2 (buffer 64 10))
    )

  (do (defonce cbus1 (control-bus 1)))

  (do (defonce vcbus1 (control-bus 1))
      (defonce vcbus2 (control-bus 1))
      (defonce vcbus3 (control-bus 1))
      (defonce vcbus4 (control-bus 1))
      )


  (do (defonce pointLength 10))
  )

                                        ;buffers


(buffer-write! buffer-64-1 [ 4 4 4 4 4 4 4 4
                             4 4 4 4 4 4 4 4
                             4 4 4 4 4 4 4 4
                             3 3 3 3 3 3 3 3
                             2 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2
                             2 2 2 2 2 2 2 2])


                                        ; single pulse point
; p=[gate, f1, f2, f3, f4, a, d, s, r, amp]
                                        ;Collection of points
(do
  (def pointBuffer (buffer (* 5 pointLength)))
  (def modValArray (writeBuffer pointBuffer (flatten [0 0 0 0 0 0 0 0 0 0
                                                      1 (into [] (map note->hz (chord :C3 :minor))) 1 0.01 0.003 0.99 0.01 1
                                                      1 (into [] (map note->hz (chord :D3 :minor))) 1 0.01 0.003 0.99 0.01 1
                                                      1 (into [] (map note->hz (chord :G3 :minor))) 1 0.01 0.003 0.99 0.01 1
                                                      1 (into [] (map note->hz (chord :Bb3 :minor))) 1 0.01 0.003 0.99 0.01 1]))))


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

;vaihtelua
(buffer-write! playBuffer [4 4 4 4 4 4 4 4
                           4 4 4 4 4 4 4 4
                           4 4 4 4 4 4 4 4
                           1 1 1 1 1 1 1 1
                           2 2 2 2 2 2 2 2
                           2 2 2 2 2 2 2 2
                           3 3 3 3 3 0 3 0
                           3 0 3 3 3 3 3 3
                           2 2 2 2 2 0 2 0
                           2 0 2 0 2 2 2 2
                           2 2 2 2 2 0 2 0
                           2 0 2 0 2 2 2 2
                           2 2 2 2 2 0 2 0
                           2 0 2 0 2 2 2 2
                           2 0 2 0 2 0 2 0
                           4 0 3 0 4 0 3 0])


(buffer-write! playBuffer (flatten [[4 4 4 4 2 2 2 2
                                     2 2 2 2 3 3 3 3
                                     4 4 4 4 2 2 2 2
                                     2 2 2 2 3 3 3 3
                                     4 4 4 4 2 2 2 2
                                     2 2 2 2 3 3 3 3
                                     4 3 3 3 2 2 2 2
                                     2 2 2 2 3 3 3 3]
                                     2 2 2 2 2 2 2 2
                                     2 3 2 3 2 3 2 3
                                     3 3 3 3 3 3 3 3
                                     1 1 1 1 1 1 1 1
                                     2 1 1 1 1 1 1 1
                                     2 1 1 2 1 2 2 1
                                     2 2 2 4 2 4 2 4
                                     4 3 4 3 4 3 4 3]))

(def playBuffer_ops (buffer 128))

; Tällä alkaa
(buffer-write! playBuffer_ops [4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               3 3 3 3 3 3 3 3
                               3 3 3 3 3 3 3 3
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2])


; hapsiaisen taakse tulee tietoisku1
(buffer-write! playBuffer_ops [3 3 3 3 3 3 3 3
                               3 3 3 3 3 3 3 3
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               4 4 4 4 4 4 4 4
                               3 3 3 3 3 3 3 3
                               3 3 3 3 3 3 3 3
                               2 2 2 2 2 2 2 2
                               2 2 2 2 2 2 2 2
                               3 3 3 3 3 3 3 3
                               3 3 3 3 3 3 3 3
                               1 1 1 1 1 1 1 1
                               1 1 1 1 1 1 1 1
                               1 1 1 1 1 1 1 1
                               1 1 1 1 1 1 1 1])

;vaihtelua
(buffer-write! playBuffer_ops [4 3 4 3 4 3 4 3
                               4 3 4 3 4 3 4 3
                               4 3 4 3 4 3 4 3
                               4 3 4 3 4 3 4 3
                               4 3 4 3 4 3 4 3
                               4 3 4 3 4 3 4 3
                               2 4 2 4 2 4 2 4
                               2 4 2 4 2 4 2 4
                               1 3 1 3 1 3 1 3
                               1 3 1 3 1 3 1 3
                               1 3 1 3 1 3 1 3
                               1 3 1 3 1 3 1 3
                               2 3 2 3 2 3 2 3
                               2 3 2 3 2 3 2 3
                               2 3 2 3 2 3 2 3
                               2 3 2 3 2 3 2 3])

(def playBuffer_kick (buffer 128))

(buffer-write! playBuffer_kick [1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 1 0 0 0 0 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 1 0 0 0])


(buffer-write! playBuffer_kick [1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0
                               1 0 1 0 1 0 1 0])


(buffer-write! playBuffer_kick [1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0])

(buffer-write! playBuffer_kick [1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 1 0 0 1 0 0
                               1 0 0 0 1 0 0 0])

(buffer-write! playBuffer_kick [1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 1 0 0 0 1 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 0 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 1 0 1 0 0 0
                               1 0 1 0 0 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 0 0 1 0 0 0
                               1 0 1 0 1 1 0 0
                               1 0 0 0 1 0 0 0])



(buffer-write! playBuffer_kick [1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0
                               1 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0])


                                        ;buffer modifiers

(def modValArray (writeBuffer pointBuffer  (setChords modValArray :E#2 :minor 1)))

(def modValArray (writeBuffer pointBuffer (setADSR modValArray 0.01 0.3 0.99 0.1 4)))

(def modValArray (writeBuffer pointBuffer (setAmp modValArray 1 3)))

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


                                        ; reader for mcsynth
(def mcsr (playReader :play-buf playBuffer :point-buf pointBuffer :in-bus-ctr b8th_beat-cnt-bus :outbus mcbus1))


;Tämä hapsiaisen kanssa
(ctl mcsr :point-buf pointBuffer :play-buf buffer-64-1 :in-bus-ctr b8th_beat-cnt-bus)

;Tämä kunhan alkaa tulla hapsiaista ja sormileikkiä
(ctl mcsr :point-buf pointBuffer :play-buf playBuffer :in-bus-ctr b8th_beat-cnt-bus)

(ctl mcsr :point-buf pointBuffer :play-buf playBuffer_ops :in-bus-ctr b4th_beat-cnt-bus)


(kill mcsr)

                                        ;reader for overpad
;Homma alkaa overpadilla
(def opsr (playReader :play-buf playBuffer_ops :point-buf pointBuffer :in-bus-ctr b16th_beat-cnt-bus :outbus mcbus2))

;alkaa tällä
(ctl opsr :play-buf playBuffer_ops :in-bus-ctr b8th_beat-cnt-bus)


;vähän vaihtelua tempolla
(ctl opsr :play-buf playBuffer_ops :in-bus-ctr b4th_beat-cnt-bus)


(control-bus-set! master-rate-bus (* 2 36))


                                        ;Reader for kick drum

(def kickr (playReader :play-buf playBuffer_kick :point-buf pointBuffer :in-bus-ctr b8th_beat-cnt-bus :outbus mcbus3))

(kill kickr)

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
          ;_          (out:kr ctrl-output ctrl-f-env)
          s1         (* osc1-level (select osc1 osc-bank-1))
          s2         (* osc2-level (select osc2 osc-bank-2))
          s3         (* osc3-level (select osc3 osc-bank-3))
          filt       (moog-ff (+ s1 s2 s3) (* cutoff 1) 3)
          filt       (free-verb filt f-env2 1.0 1)
          fout       (select:kr (= gate 0.0) [freq1 (in:kr ctrl-output)])
          _          (out:kr ctrl-output fout)
          ]
      (out 0 (pan2 (* amp amp-env filt)))))

(do (kill mcs1)
    (def mcs1 (mcsynth [:tail early-g]  :control-bus mcbus1 :amp 0.5 :osc1 2 :osc2 0 :ctrl-output vcbus1))

    )

(control-bus-get mcbus1)


(ctl mcs1 :amp 1 :osc1 0 :osc2 0 :osc3 0 :cutoff 800 :ctrl-output vcbus1 :amp 1)

(ctl mcs1 :amp 1 :osc1 0 :osc2 0 :osc3 2 :cutoff 200 :ctrl-output vcbus1 :amp 1)


(ctl mcs1 :amp 0.5)
(kill mcs1)

(kill 60)

(pp-node-tree)
                                        ;overpad
(definst overpad
  [control-bus 0 note 30 amp 0.5 outbus 0 ctrl-output 0]
  (let [control_in   (in:kr control-bus 10)
        gate  (select:kr 0 control_in)
        freq  (select:kr 1 control_in)
        a     (select:kr 5 control_in)
        d     (select:kr 6 control_in)
        s     (select:kr 7 control_in)
        r     (select:kr 8 control_in)
        noise (pink-noise)
        env    (env-gen (adsr a d s r) :gate gate)
        f-env (+ freq (* 30 freq (env-gen (perc 30.012 (- r 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (saw [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* noise freq 1.01)]) f-env)))
        ctrl-out (a2k (* env sig))
        _        (out:kr ctrl-output ctrl-out)
        audio (* amp env sig)
        ]
    (out outbus (pan2 audio))))

(do (kill op)
    (def op (overpad  [:tail early-g] :control-bus mcbus2 :ctrl-output vcbus2 :amp 1))

    )


(ctl op :amp 0.01)

(kill op)

(control-bus-get mcbus2)

(pp-node-tree)



(pp-node-tree)

                                        ;Kick
(defsynth kick [freq 80
                amp 1
                amp_output 1
                v1 0.001
                v2 0.001
                v3 0.001
                c1 -20
                c2 -8
                c3 -8
                d1 1
                d2 1
                d3 1
                f1 80
                f2 1
                f3 80
                clipVal 0.3
                control-bus 0
                video-control-bus 0
                outbus 0]
    (let [control_in   (in:kr control-bus 10)
          gate  (select:kr 0 control_in)
          freq  (select:kr 1 control_in)
          a     (select:kr 5 control_in)
          d     (select:kr 6 control_in)
          s     (select:kr 7 control_in)
          r     (select:kr 8 control_in)
          pls   gate
          adj       (max 1 pls)
          co-env    (perc v1 d1 f1 c1)
          a-env     (perc v2 d2 f2 c2)
          osc-env   (perc v3 d3 f3 c3)
          cutoff    (lpf (pink-noise) (+ (env-gen co-env :gate pls) (* 1 20)))
          sound     (lpf (sin-osc (+ 0 (env-gen osc-env :gate pls) 20)) (* 200 1))
          env       (env-gen a-env :gate pls)
          venv      (a2k env)
          _         (out:kr video-control-bus venv)
          output    (*  amp (+ cutoff sound) env)
          output    (free-verb output 0.1 0.3 0.1)
          ;output    (breakcore 0 0 0 0.5)
          ]
      (out outbus (pan2 (* amp_output (clip:ar output clipVal))))))


(do (kill k1)
    (def k1 (kick :control-bus mcbus3))

    )

(ctl k1 :amp 1 :control-bus mcbus3 :video-control-bus vcbus3
     :v1 0.01 :v2 0.01 :v3 0.01
     :d1 1 :d2 10 :d3 1
     :f1 50 :f2 5 :f3 40
     :c1 -20 :c2 -18 :c3 -18 )

(ctl k1 :amp 1 :control-bus mcbus3 :video-control-bus vcbus3
     :v1 0.01 :v2 0.001 :v3 0.001
     :d1 1 :d2 1 :d3 1
     :f1 50 :f2 5 :f3 40
     :c1 -20 :c2 -8 :c3 -8 )


(ctl k1 :amp 0.015)

(stop)


                                        ;Video
(t/start "./b13.glsl" :width 1920 :height 1080 :cams [0 2] :videos ["../videos/tietoisku_1_fixed.mp4" "../videos/spede_fixed.mp4"  "../videos/vt2.mp4" "../videos/hapsiainen_fixed.mp4" "../videos/sormileikit.mp4"])



(defonce beat-cnt-bus-atom_1 (bus-monitor b1st_beat-cnt-bus))


                                        ;indices 0-50 for control, indiced 51-100 for video selection

(add-watch beat-cnt-bus-atom_1 :cnt (fn [_ _ old new]
                                    (let [])
                                      (t/set-dataArray-item 0 (+ (nth (control-bus-get vcbus1) 0) 0.01) )
                                      (t/set-dataArray-item 1 (+ (nth (control-bus-get vcbus2) 0) 0.01) )
                                      (t/set-dataArray-item 2 (+ (nth (control-bus-get vcbus3) 0) 0.01) )
                                      ;(t/set-dataArray-item 0 (+ (nth (control-bus-get vcbus1) 0) 0.01) )
                                      ;(t/set-dataArray-item 0 (+ (nth (control-bus-get vcbus1) 0) 0.01) )

                                      ))

(control-bus-get vcbus1)

(t/set-dataArray-item 51 0)


(do
                                        ;sepede 51000
  (t/bufferSection 1 0 51000)

  (t/set-active-buffer-video 1 0)

  (t/set-video-fixed 1 :fw)

  )

(t/toggle-recording "/dev/video1")

(t/stop)

(stop)

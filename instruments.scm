(define kick-drum
  (pipe> (add (pipe> triangle (hz 15))
              (pipe> sin (hz 30))
              (pipe> sin (hz 15))
              (pipe> noise
                     (volume 1/8)
                     (adsr 0 1/8 0 0)))
         (volume 1/4)
         (fm (pipe> sawtooth
                    (hz 1)
                    (volume 1)
                    (adsr 0 1/4 0 0)))
         (adsr 1/512 1/2 0 0)))

(define bass-drum
  (pipe> sin
         (hz 30)
         (fm (pipe> sin
	            (detune 17)
	            (offset 64)
	            (adsr 1/64 1/8 0 0)))
         (adsr 1/64 1/4 0 0)
         (volume 1/2)))

(define snare-drum
  (pipe> (add (pipe> triangle
                     (adsr 0 1/2 0 0)
                     (hz 500))
	      (pipe> noise (hz 500))
	      (pipe> noise (hz 550))
	      (pipe> noise (hz 600)))
         (fm (pipe> sin (adsr 0 1 0 0) (offset 1)))
         (adsr 1/32 2/3 0 0)
         (volume 1/16)))

(define snare2
  (pipe> (add (pipe> triangle (adsr 0 1/2 0 0) (hz (* (root) 3)))
	      (pipe> noise (hz 122))
	      (pipe> noise (hz 333))
	      (pipe> noise (hz 455)))
         (fm (pipe> sin (adsr 0 1 0 0) (offset 1)))
         (adsr 1/32 2/3 0 0)
         (volume 1/16)))

(define high-hat
  (pipe> noise
         (adsr 1/32 1/2 0 0)
         (hz 5300)
         (volume 1/16)))

(define arp-lead
  (pipe> (harmonics 2 4 5 6 8)
         (volume 2/3)
         (add sin)
         (volume 1/8)
         (adsr 0 1/2 1/24 3)
         (detune 4)))

(define bell
  (pipe> (harmonics 3/2 5/2 7/2)
         (volume 1/2)
         (add sin)
         (adsr 0 2 0 3)
         (am (pipe> (harmonics 1 2 3 5 7 9)
                    (volume 1/8)
                    (offset 1/2)))
         (volume 1/12)))

(define pluck-bass
  (pipe> (harmonics 4 5 6)
         (am (pipe> (harmonics 3 4 5)
                    (detune 1/4)
                    (adsr 0 1 0 0)))
         (add (harmonics 1/2 1/4 1/8)
              (detune triangle 1/4))
         (volume 1/5)
         (adsr 1/16 1/4 1/8 0)))

(define wobble-strings
  (pipe> (add sin
              (pipe> sin (detune 2) (volume 1/2))
              (pipe> sin (detune 4) (volume 1/4)))
         (am (pipe> sin
                    (detune 1/16)
                    (volume 1/16)
                    (offset 1)))
         (adsr 1/3 1/2 1/8 3)))

(define triangle-pad
  (pipe> triangle
         (am (pipe> sin
                    (detune 1/512)
                    (volume 1/16)
                    (offset 1)))
         (adsr 1/3 1/2 1/8 3)))

(define saw-pad
  (pipe> (add (pipe> sawtooth (detune 255/254))
              (pipe> sawtooth (detune 253/254)))
         (am (pipe> sin
                    (hz 1/3)
                    (volume 1/8)
                    (offset 1)))
         (volume 1/8)
         (adsr 1/2 1/2 1/2 3)))

(define thick-pad
  (pipe> (add sin
              (pipe> sin (detune 1/2) (volume 1/2))
              (pipe> sin (detune 1/4) (volume 1/4))
              (pipe> sin (detune 3/1) (volume 1/81)))
         (am (pipe> sin
                    (hz 3)
                    (volume 1/8)
                    (offset 1)))
         (adsr 1/2 1/2 1/16 3)))

(define thick-lead
  (add (pipe> triangle
              (detune 2)
              (fm (pipe> sin
                         (detune 3/2)
                         (volume 1/3200)
                         (offset 1)))
              (adsr 1/32 1 1/32 3))
       (pipe> sin
              (adsr 1/16 1 1/64 3))))

(define fm-pad
  (pipe> sin
         (add (pipe> sin (detune 1/4)))
         (fm (pipe> sin
                    (volume 1/2048)
                    (adsr 0 1 1/2 1)
                    (hz 4)
                    (offset 1))
             (pipe> sin
                    (detune 1/2)
                    (volume 1/3048)
                    (adsr 0 1 2/3 3)
                    (offset 1)))
         (adsr 2/3 1 1/8 3)))

(define thick-fm-pad
  (pipe> (add (pipe> triangle (detune 1/2))
              (pipe> triangle)
              (pipe> sin (detune 3/2) (volume 1/16))
              (pipe> sin (detune 30/21) (volume 1/32))
              (pipe> sin (detune 31/20) (volume 1/64))
              (pipe> sin (detune 32/18) (volume 1/128)))
         (volume 1/12)
         (fm (pipe> sin
                    (hz 3)
                    (volume 1/1024)
                    (adsr 1 0 1 4)
                    (pm (pipe> sin (hz 4) (volume 2)))
                    (offset 1)))
         (adsr 1/16 1/3 1/3 4)))

(define pingy-lead
  (pipe> sin
         (detune 2)
         (volume 1/2)
         (fm (pipe> sin
                    (volume 1/64)
                    (detune 1/2)
                    (adsr 0 2 0 1)
                    (offset 1)))
         (adsr 1/128 1/3 1/16 1)))

(define isnt-lead
  (pipe> sin
         (fm (pipe> sin
                    (hz 3)
                    (volume 1/1024)
                    (adsr 1 0 1 4)
                    (offset 1)))
         (adsr 0 1/16 1/8 2)))

(define wet-lead
  (pipe> sin
         (detune 1)
         (volume 2)
         (fm (pipe> sin
                    (volume 1/128)
                    (adsr 0 1/4 1/128 0)
                    (offset 31/32)
                    (detune 3/2)))
         (rm (pipe> sin (hz (/ (root) 8))))
         (add (pipe> sin
                     (detune 4)
                     (volume 1/4)))
         (adsr 1/16 1/4 1/32 1)
         (add (pipe> sin
                     (detune 1/2)
                     (adsr 1/32 1/16 1/2 1))
              (pipe> sin
                     (volume 1/16)
                     (detune 4)
                     (fm (pipe> sin
                                (hz 3)
                                (volume 1/128)
                                (adsr 1/2 0 1 1)
                                (offset 1)))
                     (adsr 1/32 1/16 1/8 1)))
         (am (pipe> sin
                    (volume 1/2)
                    (adsr 1/2 0 1 1)
                    (offset 1)
                    (hz 3)))))

(define fm-pad
  (pipe> sin
         (volume 1/8)
         (detune 2)
         (fm (pipe> sin
                    (volume 1/512)
                    (adsr 1/2 0 1 0)
                    (hz 3)
                    (offset 1)))
         (adsr 1/2 1/2 1/4 1)))

(define fm-triangle-bass
  (pipe> triangle
         (detune 1/2)
         (pm (pipe> sin
                    (detune 1/2)
                    (volume 2)
                    (adsr 1/32 1/8 1/8 0)))
         (add (pipe> sin (detune 1/2)))
         (fm (pipe> sin
                    (volume 1/27)
                    (detune 1/2)
                    (adsr 0 1/2 0 0)
                    (offset 2/3)))
         (volume 1/2)
         (adsr 1/24 1/2 0 0)))

(define shiny-sin
  (pipe> sin
         (adsr 1/32 2/3 1/8 1/6)
         (add (pipe> sin
                     (detune 2)
                     (volume 1/4)
                     (adsr 1/8 1 1/2 1))
              (pipe> sin
                     (detune 4)
                     (volume 1/8)
                     (adsr 4 1/2 1/2 2)))))

(define pretty-pad
  (pipe> sin
         (adsr 1/32 2/3 1/8 1/6)
         (add (pipe> sin
                     (detune 2)
                     (volume 1/4)
                     (adsr 1/8 1 1/2 1))
              (pipe> sin
                     (detune 4)
                     (volume 1/8)
                     (adsr 4 1/2 1/2 2)))))

(define thunk-pluck
  (pipe> sin
         (fm (pipe> sin
                    (volume 1/121)
                    (adsr 1/2 1/2 0 0)
                    (am (pipe> sin
                               (hz 4)))
                    (offset 1)))
         (adsr 1/128 1 1/16 2/3)))

(define square-lead
  (pipe> square
         (adsr 1/32 1/2 1/256 1/32)
         (fm (pipe> sin
                    (detune 1/256)
                    (volume 1/32)
                    (offset 1)))
         (volume 1/32)))

(define kick-drum2
  (pipe> (add (pipe> triangle (hz 15))
              (pipe> sin (hz 30))
              (pipe> sin (hz 15))
              (pipe> noise
                     (volume 1/8)
                     (adsr 0 1/8 0 0)))
         (volume 1/4)
         (fm (pipe> sawtooth
                    (hz 1)
                    (volume 1)
                    (adsr 0 1/4 0 0)))
         (adsr 1/512 1/2 0 0)))

(define ducked-bass
  (pipe> (add (pipe> sawtooth (detune 1/8) (volume 1/4))
              (pipe> triangle (detune 1/4))
              (pipe> sin (detune 1/8))
              (pipe> sin (detune 3/4)))
         (am (pipe> sin (hz 1/2)))
         (volume 1/8)
         (adsr 1 1/4 1/32 1/8)))

(define soft-chords
  (pipe> sin
         (adsr 0 1/32 1/8 1)
         (fm (pipe> sin (hz 1/3)
                    (volume 1/32)
                    (offset 1)))
         (volume 1/32)))

(define whirling-in-rags-pad
  (pipe> triangle
         (add (pipe> sin (detune 1/2) (volume 1/4)))
         (adsr 5/11 6/11 1/2 2)
         (fm (pipe> sin
                    (detune 5)
                    (adsr 1/16 1/4 1/512 2)
                    (volume 1/512)
                    (offset 1)))))

(define mrzek-pluck
  (pipe> (add (pipe> sin (volume 2/3))
              (pipe> sin (volume 1/6) (detune 1/2))
              (pipe> sin (volume 1/6) (detune 2/1))
              (pipe> sin (volume 1/100) (detune 3))
              (pipe> sin (volume 1/1000) (detune 5)))
         (adsr 1/50 2/3 1/2 4)
         (am (pipe> sin (volume 1/100) (offset 1) (detune 1/32)))))

(define absolutely-lead
  (pipe> sin
         (adsr 1/16 1/2 1/16 4)
         (rm (pipe> sin
                    (adsr 1/2 1/2 1 4)))
         (add (pipe> sin
                     (adsr 1/2 1/2 0 0)
                     (detune 1/2)))
         (fm (pipe> sin
                    (volume 1/144)
                    (adsr 0 1/2 0 0)
                    (offset 1)))))

(define old-bee-pad
  (pipe> (add (pipe> sawtooth)
              (pipe> sawtooth
                     (volume 1/2)
                     (fm (pipe> sin
                                (volume 1/32)
                                (offset 1)
                                (detune 1/2)))))
         (adsr 1/8 1/2 1/2 1/2)
         (am (pipe> sin
                    (hz 2/3)
                    (fm (pipe> sin (hz 1/2)))))))

(define morning-lead
  (pipe> sin
         (detune 2)
         (add (pipe> sin (volume 1/2)))
         (adsr 0 4/5 1/16 1)
         (fm (pipe> sin
                    (volume 1/125)
                    (adsr 0 1 0 0)
                    (offset 1)
                    (hz 8)))))

(define morning-bass
  (pipe> (add (pipe> triangle
                     (adsr 1/2 1/2 0 0)
                     (detune 1/2))
              (pipe> sin
                     (adsr 9/10 1/10 0 0)
                     (detune 1/4))
              (pipe> triangle
                     (adsr 1/4 3/4 0 0)
                     (detune 2)
                     (volume 1/8)))
         (rm (pipe> sin (hz 8)))))

(define sleepy-saw-pad
  (pipe> sawtooth
         (volume 1/40)
         (adsr 4/5 1/10 1/3 2)
         (detune 1/2)
         (am (pipe> sin
                    (volume 1/50)
                    (adsr 1/32 31/32 2/3 0)
                    (detune 3)))))

(define sleepy-sin-pad
  (pipe> sin
         (volume 1/8)
         (adsr 1/32 1/2 1/12 3)
         (detune 1/2)
         (fm (pipe> sin
                    (volume 1/320)
                    (adsr 2/3 1/3 1/2 2)
                    (detune 11/8)))))

(define mush-pad
  (pipe> sin
         (volume 1/3)
         (adsr 1/3 1/8 1/2 1)
         (fm (pipe> sawtooth
                    (volume 1/320)
                    (adsr 1/8 1/32 3/4 0)
                    (detune 1/2)))))

(define eleven-pad
  (pipe> sin
         (volume 1/9)
         (rm (pipe> (add (detune sin 12/44)
		         (detune sin 13/44)
		         (detune sin 11/44))
	            (volume 1/3)
	            (adsr 0 1 0 0)
	            (offset 1/2)))
         (adsr 1/2 1/2 0 0)))

(define eleven-pluck
  (pipe> sin
         (fm (pipe> (add (detune sin 11/16)
		         (detune sin 12/16)
		         (detune sin 13/16))
	            (volume 1/48)
	            (adsr 0 1/2 0 0)))
         (adsr 0 1/2 0 0)))

(define eleven-bass
  (pipe> sin
         (detune 1/4)
         (fm (pipe> (add (detune sin 11/22)
		         (detune sin 17/22))
	            (volume 1/48)
	            (adsr 1/2 1/2 0 0)
	            (offset 1/2)))
         (adsr 0 1/2 0 0)))

(define forz-bass
  (pipe> sin
         (adsr 1/32 1/8 1/4 4)
         (fm (pipe> sin
	            (detune 1/2)
	            (volume 1/64)
	            (adsr 1/2 1/2 0 0)
	            (offset 1)))))

(define greensleeves-lead
  (pipe> sin
         (rm (pipe> sin (detune 3)
	            (adsr 0 1/4 0 0)
	            (volume 2/3)
	            (offset 1))
	     (pipe> sin (detune 5)
	            (adsr 0 1/2 0 0)
	            (volume 2/3)
	            (offset 1))
	     (pipe> sin (detune 7)
	            (adsr 0 1/6 0 0)
	            (volume 1/2)
	            (offset 1))
	     (pipe> sin (detune 9)
	            (adsr 0 2/3 0 0)
	            (volume 1/2)
	            (offset 1)))
         (detune 2)
         (adsr 1/8 7/8 0 0)))

(define greensleeves-chords
  (pipe> sin
         (rm (pipe> sin
	            (detune 1/2)
	            (volume 1/12)
	            (adsr 0 1 0 0)
	            (offset 1))
	     (pipe> sin
	            (detune 3/2)
	            (volume 1/12)
	            (adsr 0 1 0 0)
	            (offset 1))
	     (pipe> sin
	            (detune 5/2)
	            (volume 1/12)
	            (adsr 0 1 0 0)
	            (offset 1))
	     (pipe> sin
	            (detune 7/2)
	            (volume 1/12)
	            (adsr 0 1 0 0)
	            (offset 1)))
         (adsr 1/32 31/32 0 0)))

(define draguler
  (pipe> sin
         (fm (pipe> sin (volume 1/3) (detune 2/3))
	     (pipe> sin (volume 1/4) (detune 1/2)))
         (add (pipe> sawtooth (volume 1/3) (detune 3)))
         (am (pipe> sin (detune 3/2)))
         (adsr 1/8 1/2 0 0)))

(define frog-lead
  (pipe> sin
         (detune 2)
         (add (pipe> sin (detune 3) (volume 1/9))
	      (pipe> sin (detune 9) (volume 1/27)))
         (am (pipe> sin (detune 1/16) (offset 1))
	     (pipe> sin (detune 3/16) (offset 1))
	     (pipe> sin (detune 3) (offset 1)))
         (volume 1/128)
         (adsr 1/2 1/4 0 0)))

(define kick-drum3
  (pipe> sin
         (hz 30)
         (add (pipe> noise (hz 120) (volume 1/32) (adsr 0 1/2 0 0))
	      (pipe> noise (hz 240) (volume 1/32) (adsr 0 1/4 0 0))
	      (pipe> noise (hz 360) (volume 1/32) (adsr 0 1/8 0 0)))
         (fm (pipe> sin
	            (detune 17)
	            (offset 64)
	            (adsr 1/64 1/8 0 0)))
         (adsr 1/64 2/3 0 0)
         (volume 1/2)))

(define snare-drum3
  (pipe> (add (pipe> triangle (adsr 0 1/2 0 0) (hz (* (root) 3)))
	      (pipe> noise (hz 122))
	      (pipe> noise (hz 333))
	      (pipe> noise (hz 455)))
         (fm (pipe> sin (adsr 0 1 0 0) (offset 1)))
         (adsr 1/32 2/3 0 0)
         (volume 1/16)))

(define frog-bass
  (pipe> sin
         (fm (pipe> sin (detune 1/2) (volume 1/128)
	            (adsr 0 1 0 0) (offset 1)))
         (rm (pipe> sin (detune 1/2)
	            (volume 1/8)
	            (adsr 0 1 0 0)
	            (offset 1)))
         (am (pipe> sin (detune 1/4)
	            (volume 1/16)
	            (adsr 1/2 1/2 0 0)
	            (offset 1)))
         (volume 1/4)
         (adsr 1/128 1 0 0)))

(define frog-stabs
  (pipe> sin
         (detune 2)
         (rm (pipe> sin (detune 5)
	            (volume 1/32)
	            (adsr 0 1 0 0)
	            (offset 1)))
         (volume 1/8)
         (adsr 1/32 1/2 0 0)))

(define california-pad
  (pipe> sin
         (add (pipe> sin (volume 1/4) (detune 1/4)))
         (fm (pipe> sin
	            (detune 1/4)
	            (volume 1/32)
	            (adsr 1/8 1/8 0 0)
	            (offset 1)))
         (adsr 1/9 31/32 0 0)))

(define (coins-snare)
  (pipe> (add (pipe> triangle (hz (/ (root) 2)) (adsr 0 1/8 0 0))
	      (pipe> noise (hz (* (root) 3)) (adsr 0 1/8 0 0))
	      (pipe> noise (hz (* (root) 5)) (adsr 0 1/4 0 0))
	      (pipe> noise (hz (* (root) 7)) (adsr 0 1/2 0 0)))
         (adsr 1/32 1 0 0)
         (volume 1/24)))

(define (coins-kick)
  (pipe> sin
         (hz (/ (root) 8))
         (add (pipe> noise
	             (hz 50)
	             (volume 1/32)
	             (adsr 0 1/16 0 0)))
         (fm (pipe> sin
	            (detune 17)
	            (offset 64)
	            (adsr 1/64 1/8 0 0)))
         (adsr 1/64 1/4 0 0)
         (volume 1)))

(define rhythm-pluck
  (pipe> (add sin (detune sin 2) (detune sin 4))
         (fm (pipe> sin
	            (volume 1/128)
	            (adsr 0 1/4 0 0)
	            (offset 1)
	            (detune 3))
	     (pipe> sin
	            (volume 1/256)
	            (adsr 0 1/8 0 0)
	            (offset 1)
	            (detune 4))
	     (pipe> sin
	            (volume 1/512)
	            (adsr 0 1/16 0 0)
	            (offset 1)
	            (detune 5)))
         (adsr 0 1 0 0)
         (volume 1/64)))

(define rhythm-kick
  (pipe> sin
         (hz 28)
         (add (pipe> noise
	             (hz 50)
	             (volume 1/32)
	             (adsr 0 1/16 0 0)))
         (fm (pipe> sin
	            (detune 17)
	            (offset 64)
	            (adsr 1/64 1/8 0 0)))
         (adsr 1/64 1/4 0 0)
         (volume 1/2)))

(define rhythm-snare
  (pipe> (add (pipe> triangle (hz 115) (adsr 0 1/8 0 0))
	      (pipe> noise (hz 480) (adsr 0 1/8 0 0))
	      (pipe> noise (hz 240) (adsr 0 1/4 0 0))
	      (pipe> noise (hz 120)))
         (adsr 1/32 1 0 0)
         (volume 1/24)))

(define rhythm-pad
  (pipe> (add (pipe> sin)
	      (pipe> sin (detune 3) (adsr 0 1/3 0 0))
	      (pipe> sin (detune 4) (adsr 0 1/4 0 0))
	      (pipe> sin (detune 5) (adsr 0 1/5 0 0)))
         (adsr 1/8 3/4 0 0)
         (volume 1/81)
         (fm (pipe> sin
	            (adsr 0 1/2 0 0)
	            (volume 1/64 1/2)
	            (offset 1)
	            (detune 5 1/4 3)))))

(define smokey-pluck
  (pipe> sin
         (rm (pipe> triangle
	            (detune 1/2))
	     (pipe> triangle
	            (detune 1/3))
	     (pipe> triangle
	            (detune 1/5)))
         (adsr 1/64 9/10 1/2 1/8)))

(define bustifer-pluck
  (pipe> sin
         (detune 255/256)
         (add (detune sin 256/255))
         (fm (pipe> sin (volume 1/256) (adsr 0 1 0 0) (detune 3/2)))
         (detune 1/2)
         (adsr 1/256 1 1/2 2)))

(define bustifer-bass
  (pipe> sawtooth
         (adsr 1/32 1/2 0 0)
         (add (pipe> sin
	             (detune 1/2)
	             (volume 1/3)
	             (adsr 1/32 2/3 0 0)))))

(define dark-chords
  (pipe> sin
         (fm (pipe> sin
	            (volume 1/16) (detune 3) (offset 1)))
         (am (pipe> sin (hz 7) (volume 1/2) (offset 1)))
         (adsr 1/3 1 1/8 3)))

(define wowie-pad
  (pipe> sin (adsr 1 0 1 1/2)
         (am (pipe> sin (detune 5/8))
	     (pipe> sin (detune 7/8)))
         (rm (pipe> sin (hz 3)))
         (detune 2)
         (volume 1/16)))

(define wowie-bass
  (pipe> sin
         (fm (pipe> sin (detune 1/4) (volume 5)
	            (offset 1))
	     (pipe> sin (detune 3/2)
	            (volume 1/4)
	            (offset 1)))
         (adsr 1/32 2/3 0 0)
         (volume 1/6)))

(define wowie-hat
  (pipe> noise
         (adsr 1/32 1/2 0 0)
         (hz 2300)
         (volume 1/4)))

(define wowie-snare
  (pipe> (add (pipe> noise (hz 222))
	      (pipe> noise (hz 333))
	      (pipe> noise (hz 555)))
         (adsr 1/32 1/4 0 0)
         (volume 1/8)))

(define wowie-kick
  (pipe> sin
         (hz 30)
         (fm (pipe> sin
	            (detune 17)
	            (offset 64)
	            (adsr 1/64 1/8 0 0)))
         (adsr 1/64 1/4 0 0)
         (volume 1/2)))

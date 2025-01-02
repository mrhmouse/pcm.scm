(import (pcm)
        (chezscheme))

(sample-rate 16000)

(include "scales.scm")
(include "instruments.scm")

(define-syntax in-place
  (syntax-rules ()
    ((in-place body ...)
     (parameterize ((tape-head (tape-head)))
       body ...))))

(define (simultaneously first-track . more-tracks)
  (let ((final-head
         (parameterize ((tape-head (tape-head)))
           (first-track)
           (tape-head))))
    (cond ((null? more-tracks)
           (tape-head final-head)
           (values))
          (else (apply simultaneously more-tracks)))))

(define (swing grid amount)
  (groove (* 2 grid amount)
          (* 2 grid (- 1 amount))))

(define-song "2024-12-27-hungry-for-sofritas"
  (define scale '#(1 12/11 13/11 14/11 15/11 16/11 17/11 18/11 19/11 20/11 21/11 2))
  (define p (snap-to-scale play scale))
  (root 150)
  (tempo 50)
  (render-tail 1)
  (for-each
   (lambda (tetrad)
     (simultaneously
      (lambda ()
        (voice (pipe>
                frog-bass (detune 1/4) (volume 1/2)
                (add (pipe> frog-bass (detune 1/2) (volume 1/8)))))
        (groove 1/4)
        (repeat 4 (for-each p (sort < tetrad))
                (for-each p (cdr (sort > tetrad)))
                (play #f)))
      (lambda ()
        (voice (pipe> kick-drum3 (volume 1/4)))
        (groove 1/4)
        (repeat 8 (play 1 #f 1 #f)))
      (lambda ()
        (voice (pipe> snare-drum (volume 1/4) (hz 4900)))
        (swing 1/4 2/3)
        (repeat 8 (play #f #f 1 #f)))
      (lambda ()
        (voice (pipe> high-hat2 (volume 1/8)))
        (swing 1/8 3/5)
        (repeat (* 8 8) (play 1)))
      (lambda ()
        (voice (pipe> soft-chords
                      (adsr 1/4 0 1 1)
                      (fm (pipe> sin (volume 1/128)
                                 (detune 1/2)
                                 (adsr 1/2 1/2 1/8 1)
                                 (offset 1)))))
        (swing 1/4 2/3)
        (repeat (* 2 8) (p (car (sort > tetrad)) tetrad)))
      (lambda ()
        (voice (pipe> soft-chords (detune 2)
                      (volume 1/2)
                      (fm (pipe> soft-chords
                                 (detune 1/4)
                                 (volume 1/64)
                                 (adsr 1/2 1/2 1/8 1)
                                 (offset 1)))))
        (groove 4 1 3)
        (p #f (* 2 (car (sort < tetrad))) (car (sort > tetrad))))
      (lambda ()
        (voice (pipe> sin
                      (adsr 1/2 0 1 1)
                      (fm (pipe> sin (volume 1/1024)
                                 (detune 3/2)
                                 (adsr 1/2 0 1 1)
                                 (offset 1))
                          (pipe> sin (volume 1/1024)
                                 (detune 9/4)
                                 (adsr 0 1 0 0)
                                 (offset 1)))
                      (am (pipe> sin (volume 1/2)
                                 (hz 5/6)
                                 (offset 1/2)))
                      (volume 1/1536)))
        (groove 4)
        (for-each p (map (lambda (n)
                           (list n (* 4/3 n) (* 16/9 n)))
                         (cddr (sort < tetrad)))))))
   '((1 3/2 15/8 5/2)
     (1 3/2 15/8 9/4)
     (1 5/3 2 5/2)
     (9/8 4/3 5/3 9/4)
     (1 4/3 5/3 5/2)
     (15/16 5/4 3/2 9/4)
     (1 3/2 16/9 12/5)
     (1 3/2 15/8 5/2)
     (1 4/3 5/3 9/4)
     (1 7/5 9/5 12/5)
     (1 3/2 15/8 5/2))))

(define-song "2024-12-25-lost-in-your-woods"
  (define scale (edi 3/2 4))
  (define the-path
    (map (lambda (chord)
           (map (lambda (n) (snap-note-to-scale n scale))
                chord))
         '((1 5/4 3/2 15/8)
           (1 5/4 3/2 5/3)
           (1 9/8 4/3 5/3)
           (9/8 4/3 3/2 15/8))))
  (define (move-next from-whence)
    (let ((next (cdr from-whence)))
      (if (pair? next) next
          (let ((next-path (map new-path the-path)))
            (set! the-path next-path)
            next-path))))
  (define (new-path old-path)
    (map (lambda (n)
           (interval-reduce (snap-note-to-scale (* 4 n) scale) 2))
         old-path))
  (root 222)
  (render-tail 1/2)
  (do ((i 0 (+ i 1))
       (i-am-here the-path (move-next i-am-here)))
      ((= i 24))
    (call-with-values
        (lambda () (apply values (sort < (car i-am-here))))
      (lambda (a b c d)
        (simultaneously
         (lambda ()
           (voice (pipe> kick-drum
                         (volume 16)))
           (groove 1/4)
           (repeat 16 (play 1 #f)))
         (lambda ()
           (voice (pipe> sub-bass (volume 8)))
           (groove 1/2)
           (repeat 8 (play a)))
         (lambda ()
           (voice (pipe> pluck-bass
                         (volume 12)))
           (swing 1/4 2/3)
           (repeat
            2 (play #f a b c
                    #f a b #f
                    #f a b c
                    #f a #f a)))
         (lambda ()
           (voice (pipe> high-hat
                         (volume 6)))
           (swing 1/4 2/3)
           (repeat 8 (play #f #f 1 1)))
         (lambda ()
           (voice triangle-pad)
           (groove 1/2)
           (repeat 8 (play #f (car i-am-here))))
         (lambda ()
           (voice triangle-pad)
           (swing 1/4 2/3)
           (play a #f b a
                 #f a c b
                 a #f b a
                 d #f b c
                 #f c d c
                 b #f c b
                 c #f d c
                 d c b #f))
         (lambda ()
           (voice (pipe> isnt-lead (volume 3)))
           (swing 1 2/3)
           ((snap-to-scale play scale)
            #f (* 2 a) (* 2 b) (* 2 c)
            (* 2 c) (* 2 b) (* 2 b)
            (if (even? i) (* 2 a)
                (* 2 d)))))))))
(define-song "2024-12-25-doodle"
  (define L 16)
  (in-place (swing 1/8 2/3)
            (voice (pipe> noise (hz 2600)
                          (adsr 0 1/16 0 0)
                          (volume 1/8)))
            (repeat (* 8 L) (play 1)))
  (in-place (groove 1/2)
            (voice kick-drum)
            (repeat (* 2 L) (play 1)))
  (in-place (swing 1/8 3/5)
            (voice (pipe> snare-drum3 (hz 2900)))
            (repeat L (play #f #f #f #f 1 #f #f #f)))
  (in-place (voice pluck-bass)
            (swing 1/8 2/3)
            (repeat (/ L 2)
                    (play #f 3/4 1 5/4 4/3 7/5 #f 3/2)
                    (play #f 3/4 1 3/2 5/4 4/3 #f #f)))
  (in-place (voice (pipe> sub-bass
                          (adsr 0 1/2 0 0)
                          (volume 1/4)))
            (swing 1/2 2/3)
            (repeat (* 2 L) (play 1)))
  (in-place (voice soft-chords)
            (swing 1/4 2/3)
            (repeat (/ L 8)
                    (play #f #f '(3/2 15/8 9/4) #f)
                    (play #f #f '(3/2 15/8 9/4) #f)
                    (play #f #f '(3/2 15/8 9/4) #f)
                    (play #f #f '(8/5 2 12/5) '(3/2 15/8 9/4))
                    (play #f #f '(3/2 15/8 9/4) #f)
                    (play #f #f '(3/2 15/8 9/4) #f)
                    (play #f #f '(3/2 15/8 9/4) #f)
                    (play #f #f '(5/3 2 5/2) '(3/2 15/8 9/4))))
  (in-place (voice pingy-lead)
            (swing 1/8 2/3)
            ;;    xxxx xxxx xxxx xxxx  xxxx xxxx xxxx xxxx
            (play #f   1    10/9 9/8   6/5  5/4  #f   3/4
                  4/5  #f   1    #f    4/3  5/4  #f   9/8
                  #f   5/4  #f   #f    #f   #f   #f   #f
                  #f   #f   #f   #f    #f   #f   #f   #f
                  #f   1    10/9 9/8   6/5  5/4  #f   3/4
                  #f   3/4  2/3  3/4   #f   1    #f   #f
                  1/2  #f   #f   #f    #f   #f   #f   #f
                  #f   #f   #f   #f    #f   #f   #f   #f)
            ;;    xxxx xxxx xxxx xxxx  xxxx xxxx xxxx xxxx
            (play #f   1    10/9 9/8   6/5  5/4  #f   3/4
                  4/5  #f   1    #f    4/3  5/4  #f   3/2
                  #f   5/4  #f   #f    #f   #f   #f   3/4
                  4/5  #f   5/4  #f    #f   #f   #f   #f
                  #f   1    10/9 9/8   6/5  5/4  #f   3/4
                  #f   5/6  3/4  5/6   #f   1    #f   #f
                  1/2  #f   #f   #f    #f   #f   #f   #f
                  #f   #f   5/8  2/3   #f   3/4  #f   #f)))

(define-song "2024-reflections"
  (define scale (mos:citric 3/2))
  (define p (snap-to-scale play scale))
  (render-tail 2)
  (do ((k 4 (+ 1 k))
       (n 0 (+ 1 n)))
      ((= n 8))
    (do ((r (+ 1 (/ k)) (+ (* r (expt 2 n)) (/ k)))
         (chord '(9/8 4/3 3/2 15/8)
                (map (lambda (n)
                       (interval-reduce (/ r n) 2))
                     chord))
         (i 0 (+ i 1)))
        ((= i 4))
      (simultaneously
       (lambda ()
         (voice (pipe> sin
                       (detune 1/2)
                       (adsr 1/2 1/2 1/2 1)
                       (volume 1/8)))
         (groove 4)
         (p chord))
       (lambda ()
         (voice (pipe> morning-lead
                       (volume 1/8)))
         (groove 1/4)
         (for-each p (sort < chord))
         (for-each p (map (lambda (n) (* 2 n))
                          (sort < chord)))
         (for-each p (map (lambda (n) (* 2 n))
                          (sort > chord)))
         (for-each p (sort > chord)))))
    (do ((r (+ 1 (/ k)) (+ (* r (expt 2 n)) (/ k)))
         (chord '(1 5/4 3/2 15/8)
                (map (lambda (n)
                       (interval-reduce (/ r n) 2))
                     chord))
         (i 0 (+ 1 i)))
        ((= i 4))
      (simultaneously
       (lambda ()
         (voice (pipe> sin
                       (detune 1/2)
                       (adsr 1/2 1/2 1/2 1)
                       (volume 1/8)))
         (let ((wide-chord (map (lambda (n)
                                  (if (and (not (= 1 n)) (< n 3/2))
                                      (* 2 n)
                                      n))
                                chord)))
           (groove 3/2 1/2 1
                   1/2 1 3/2)
           (p #f wide-chord #f
              wide-chord #f chord)))
       (lambda ()
         (voice (pipe> morning-lead
                       (volume 1/8)))
         (case (modulo i 3)
           ((0) (groove 3 3/4 3/4 3/4 3/4))
           ((1) (groove 4 1/2 1/2 1/2 1/2))
           ((2) (groove 5 1/4 1/4 1/4 1/4)))
         (p #f)
         (for-each p (sort (if (even? i) < >) chord)))))))

(define-song "booger"
  (voice morning-lead)
  (groove 2 1)
  (render-tail 2)
  (repeat
   4 (play '(1 3/2 15/8 5/2) #f 8/3 #f 3 #f
           '(1 3/2 15/8 9/4) #f 8/3 3 8/3 5/2
           '(1 4/3 5/3 20/9) #f #f 8/3 #f 5/2
           '(3/4 9/8 27/16 81/32) #f 8/3 3 8/3 5/2))
  (rewind)
  (voice morning-bass)
  (groove 9/2)
  (repeat
   2 (play 1 1
           3/4 3/4
           2/3 2/3
           3/4 5/6
           1 1
           3/4 3/4
           27/32 2/3
           27/32 3/4)))

(define-song "should be in bed"
  (define theme
    '((1 4/3 16/9 64/27)
      (1 32/27 3/2 16/9)
      (1 4/3 27/16 16/9)
      (1 32/27 3/2 16/9)
      (1 4/3 27/16 16/9)
      (1 32/27 3/2 16/9)
      (1 9/8 4/3 27/16)
      (1 9/8 32/27 4/3)))
  (root 100)
  (render-tail 2)
  (voice (pipe> triangle-pad))
  (groove 3/4 1/4)
  (repeat 3 (for-each (lambda (chord)
                        (play chord #f chord #f
                              chord #f chord #f))
                      theme))
  (rewind (* 2 32))
  (voice (pipe> absolutely-lead
                (detune 4)))
  (groove 1/2)
  (for-each (lambda (chord)
              (play #f)
              (for-each play (sort > (map (lambda (n) (interval-reduce (* 3 n) 2))
                                          chord)))
              (for-each play (sort < (cdr chord))))
            theme)
  (for-each (lambda (chord)
              (groove 3/2 1/2)
              (play (cadr chord)
                    (car (sort > (map (lambda (n) (interval-reduce (* 27 n) 2)) chord)))
                    #f
                    (caddr chord)))
            theme)
  (rewind (* 3 32))
  (voice (pipe> morning-bass
                (detune 4)))
  (for-each (lambda (chord)
              (groove 2)
              (play 1)
              (groove 1/2)
              (for-each play (map (lambda (n) (interval-reduce n 3/2)) chord)))
            theme)
  (for-each (lambda (chord)
              (groove 1/2)
              (play 1 1 1 1)
              (groove 1/4)
              (for-each (lambda (n) (play n #f))
                        (map (lambda (n) (interval-reduce n 3/2)) chord)))
            theme)
  (for-each (lambda (chord)
              (groove 2)
              (play 1)
              (groove 1/4)
              (for-each (lambda (n) (play n #f))
                        (sort >
                              (map (lambda (n) (interval-reduce n 4/3))
                                   chord))))
            theme))

(define-song "you are asleep again"
  (define (hyp a b)
    (sqrt (+ (* a a) (* b b))))
  (voice triangle-pad)
  (play '(1 5/4 3/2 15/8)
        '(1 5/4 25/16 15/8))
  (play '(1 5/4 3/2 15/8)
        '(1 5/4 25/16 15/8))
  (play '(1 5/4 3/2 15/8)
        '(1 5/4 25/16 15/8))
  (play '(1 4/3 5/3 15/8)
        '(1 5/4 25/16 15/8))
  (do ((chords '((1 5/4 3/2 15/8)
                 (1 5/4 25/16 15/8)
                 (1 4/3 5/3 15/8)
                 (1 5/4 25/16 15/8))
               (map (lambda (chord)
                      (sort (if (even? n) < >)
                            (map (lambda (note)
                                   (interval-reduce (* 4/3 note) 2))
                                 chord)))
                    chords))
       (n 0 (+ 1 n)))
      ((= n 4))
    (for-each (lambda (chord)
                (groove 8)
                (voice (detune triangle-pad 1/2))
                (play chord)
                (rewind 6)
                (groove 2)
                (voice arp-lead)
                (for-each play (map (lambda (x) (* x 11/9)) (cdr chord))))
              chords))
  (do ((chords '((1 5/4 3/2 15/8)
                 (1 5/4 25/16 15/8)
                 (1 4/3 5/3 15/8)
                 (1 5/4 25/16 15/8))
               (map (lambda (chord)
                      (map (lambda (note)
                             (interval-reduce (* 45/32 note (expt 3 n)) 2))
                           chord))
                    chords))
       (n 0 (+ 1 n)))
      ((= n 4))
    (for-each (lambda (chord)
                (groove 8/9)
                (voice (detune triangle-pad 1/2))
                (repeat 9 (play chord))
                (rewind 8)
                (groove (* 1/2 (+ n 1))
                        (* 1/2 (- 3 n)))
                (voice (pipe> mrzek-pluck
                              (adsr 0 1/2 0 0)))
                (for-each (lambda (note)
                            (play (interval-reduce (* 3/2 11/9 note) 4/3)
                                  (interval-reduce (* 3/2 note) 4/3)))
                          (sort (if (even? n) < >)
                                chord)))
              chords)))

(define-song "bored in meeting"
  (define (q b n . factors)
    (fold-left + 0
               (map (lambda (n i)
                      (/ n (expt b (+ 1 i))))
                    (factorize n factors)
                    (iota (length factors)))))
  (do ((i 10 (+ i 1)))
      ((>= i 128))
    (let ((n (q 3/2 i 2 3 5 7 11 13 17 19 23 29 31)))
      (if (zero? n)
          (play #f)
          (play n)))))

(define-song "softly"
  (define scale (rotate-scale -2 (rank-2-temperament 3 3/2 4)))
  (define play/indexed (index-scale play scale))
  (define (p . notes-or-chords)
    (for-each (lambda (note-or-chord)
                (play/indexed (map (lambda (note)
                                     (and (number? note)
                                          (modulo (+ (m) note) 16)))
                                   (if (list? note-or-chord)
                                       note-or-chord
                                       (list note-or-chord)))))
              notes-or-chords))
  (define m (make-parameter 0))
  (define v/harm
    (pipe> sin
           (fm (pipe> (unison sin
                              1 2 1/2
                              81/80 80/81
                              121/120 120/121
                              323/322 322/323)
                      (volume 1/256)
                      (offset 1))
               (pipe> sin
                      (hz 1/3)
                      (volume 1/256)
                      (offset 1)))
           (am (pipe> sin
                      (hz 1/3)
                      (volume 1/256)
                      (offset 1)))
           (adsr 1/16 1/8 5/8 4)))
  (define v/lead
    (pipe> sin
           (detune 2)
           (adsr 1/2 1/2 1/2 1/16)
           (am (pipe> sin (volume 1/3) (offset 1)
                      (hz 3)))
           (fm (pipe> (harmonics 1 3 5 7 9 11)
                      (volume 1/1024)
                      (adsr 1 0 1 4)
                      (offset 1))
               (pipe> sin (volume 1/81) (offset 1)
                      (hz 1/3)))
           (volume 1/3)))
  (define v/pluck
    (pipe> sin
           (detune 2)
           (adsr 1/64 1/8 1/8 1/16)
           (am (pipe> sin (volume 1/3) (offset 1)
                      (hz 3)))
           (fm (pipe> (harmonics 1 3 5 7 9 11)
                      (volume 1/1024)
                      (adsr 1 0 1 4)
                      (offset 1))
               (pipe> sin (volume 1/81) (offset 1)
                      (hz 1/3)))
           (volume 3/2)))
  (define v/kick
    (pipe> kick-drum
           (add (pipe> sin
                       (detune 1/8)
                       (adsr 1/32 31/32 0 0)))
           (volume 16)))
  (root 148)
  (tempo 45)
  (render-tail 4)
  (repeat 2
          (in-place (groove 2 1 2 1 6)
                    (voice v/lead)
                    (p 0 7 6 5 4)
                    (p 0 1 2 #f 4)
                    (p 5 1 5 7 #f)
                    (p 8 7 6 5 4))
          (repeat 4
                  (in-place (groove 1)
                            (voice v/kick)
                            (play 1 1))
                  (groove 1/4)
                  (voice v/harm)
                  (p '(0 2 4) '(0 2 4) '(0 2 4) '(0 2 4)
                     '(0 2 4) '(0 2 4) '(0 2 4) 0)
                  (p '(0 2 4) '(0 2 4) '(0 2 4) '(0 2 4)
                     '(0 2 4) '(0 2 4) '(0 2 4) 3)
                  (groove 3/4 1/4 1
                          3/4 1/4 1/2 1/2)
                  (p '(4 6 8) 4 '(4 6 8)
                     '(4 6 8) 4 '(4 6 9) '(4 6 8))
                  (m (- (m) 1)))
          (in-place
           (groove 2 3/4 1/4 1
                   2 3/4 1/4 1/4 3/4
                   2 3/4 1/4 1
                   2 1 1)
           (voice v/pluck)
           (p 0 5 0 7
              0 5 0 7 4
              0 5 0 7
              0 5 4))
          (repeat 2
                  (in-place (groove 1)
                            (voice v/kick)
                            (play 1 1))
                  (groove 1 3/4 1/4)
                  (voice v/harm)
                  (p '(-1 3 7) '(3 7 11) 10
                     '(2 6 10) '(6 10 14) 13)
                  (p '(-1 3 7) '(3 7 11) 10
                     '(-2 2 6) '(2 6 11) 9)
                  (m (+ (m) 3)))))

(define-song "squid pudding"
  (define scale (rank-2-temperament 3.02 2 7))
  (define p (index-scale play scale))
  (define (poly dur . notes-and-pulses)
    (for-each (lambda (note-and-pulse)
                (parameterize ((groove (/ dur (cdr note-and-pulse)))
                               (tape-head (tape-head)))
                  (repeat (cdr note-and-pulse)
                          (p (car note-and-pulse)))))
              notes-and-pulses)
    (parameterize ((groove dur))
      (play #f)))
  (voice (pipe> sin
                (add (pipe> (harmonics 2 4 81/20)
                            (fm (pipe> (harmonics 1/2 1/3 1/5 1/7 1/11)
                                       (volume 1/1024)
                                       (adsr 1 0 1 2)
                                       (offset 1)
                                       (detune 1/2)))))
                (volume 1/2)
                (adsr 1/8 1/2 1/24 2)
                (detune 1/4)))
  (render-tail 2)
  (groove 4)
  (p '(0 2 4 6 8)
     '(1 3 5 7 9)
     '(0 2 4 7 9)
     '(1 3 5 8 10))
  (p '(0 4 6 10 12)
     '(1 5 7 10 11)
     '(3 5 7 9 11)
     '(3 7 11 13))
  (p '(0 4 8)))

(define-song "baroque paper scissors"
;;; PROTODOME cover from the album 4000ad
  (define (channel-a)
    (r 4)
    ;; paganini-esque introduction
    (m25) (r 8) (t 45)
    ;; fade-in
    (repeat
     4 (o 3) (v 2) (c 32) (r) (v 1)
     (repeat 3 (c) (r)) (v 2) (c) (r)
     (v 1) (c) (r) (v 2)
     (repeat 3 (c) (r))
     (repeat
      2 (v 1) (c) (r) (c) (r)
      (v 2) (c) (r))
     (c) (r))
    (v 3) (c 32) (r) (c) (r) (v 2)
    (repeat 9 (c) (r)) (v 3) (c) (r) (v 4) (c) (r) (v 5) (c)
    (r) (v 6) (c) (r) (v 7) (c) (r)
    ;; proper introduction
    (m2) (m2)
    ;; section A (arp)
    (repeat
     2 (repeat 4 (m2))
     (repeat
      3 (o 3) (v 4) (d+ 32) (v 2) (d+) (v 4) (c+) (v 2) (c+) (v 4)
      (g+) (v 2) (g+) (v 4) (o<) (g+) (v 2) (g+)
      (v 4) (o>) (d+) (v 2) (d+) (v 4) (f) (v 2) (f) (v 4) (g+)
      (v 2) (g+) (v 4) (o<) (g+) (v 2) (g+))
     (o 3) (v 4) (d+ 32) (v 2) (d+) (v 4) (c+) (v 2) (c+) (v 4)
     (g+) (v 2) (g+) (v 4) (o<) (g+) (v 2) (g+) (m2))
    (m2)
    ;; section B1
    (m14) (m14)
    (m15) (r 64)                        ; solo echo
    (m14) (m14)
    ;; section B2
    (repeat
     2
     (v 7) (o 5) (c 64) (o<) (b 64) (g 64) (e 64) (c 64)
     (o<) (b 64) (g 64) (e 64) (c 64) (r 48)
     (o>) (e 64) (c 64) (o<) (b 64) (g 64) (r 8) (o>)
     (e 64) (c 64) (o<) (b 64) (g 64) (v 5)
     [repeat 6 (o 4) (e 64) (c 64) (o<) (b 64) (g 64)]
     (r 16) (v 7) (o 4) (e 64) (d 64) (o<) (b 64) (g 64)
     (r 8) (o>) (e 64) (d 64) (o<) (b 64) (g 64) (r 8)
     (o>) (e 64) (d 64) (o<) (b 64) (g 64) (v 5)
     [repeat 3(o 4) (e 64) (d 64) (o<) (b 64) (g 64)]
     (v 7) (o 5) (c 32) (o<) (b 32) (g 32) (e 32)
     (d 32) (c 32) (o<) (b 32) (a 32)
     (v 7) (o>) (f 64) (d+ 64) (c 64) (o<) (g 64) (r 8)
     (o>) (f 64) (d+ 64) (c 64) (o<) (g 64) (r 8) (o>)
     (f 64) (d+ 64) (c 64) (o<) (g 64) (v 5)
     [repeat 6 (o 4) (f 64) (d+ 64) (c 64) (o<) (g 64)]
     (r 16) (v 7) (o 4) (g 64) (d+ 64) (c 64) (o<) (a+ 64)
     (r 8) (o>) (g 64) (d+ 64) (c 64) (o<) (a+ 64) (r 8)
     (o>) (f 64) (d 64) (c 64) (o<) (g 64) (v 5)
     [repeat 5 (o 4) (f 64) (d 64) (c 64) (o<) (g 64)]
     (v 8)
     [repeat 2 (o 4) (f 64) (d 64) (o<) (b 64) (g 64)]))
  (define (channel-b)
    ;; paganini-esque introduction
    (r 4)
    (r 16) (v 3) (m25) (r 16)
    ;; fade-in
    (r 1) (r)
    (repeat
     2 (v 2) (o 4) (d 32)
     [repeat 2 (v 1) (o 3) (f) (v 2) (g) (v 1) (o>) (d) (v 2) (c) (v 1)
             (o<) (g) (v 2) (f) (v 1) (o>) (c) (v 2) (d)]
     (v 1) (o<) (f) (v 2) (g) (v 1) (o>) (d) (v 2) (g) (v 1) (o<) (g)
     (v 2) (o>) (f) (v 1) (g) (v 2) (e) (v 1) (f) (v 2) (c) (v 1) (e) (v 2)
     (o<) (g) (v 1) (o>) (c) (v 2) (o<) (f) (v 1) (g))

    (v 2) (o 4) (d 32) (v 1) (o<) (f) (v 2) (g) (v 1) (o>) (d) (v 3)
    (c) (v 2) (o<) (g) (v 3) (f) (v 2) (o>) (c) (v 4) (d) (v 2) (o<)
    (f) (v 4) (g) (v 2) (o>) (d) (v 4) (c) (v 2) (o<) (g) (v 4) (f) (v 2)
    (o>) (c) (v 4) (d) (v 2) (o<) (f) (v 4) (g) (v 2) (o>) (d) (v 4)
    (g) (v 2) (o<) (g) (v 4) (o>) (f) (v 2) (g) (v 4) (e) (v 2) (f)
    (v 4) (c) (v 2) (e) (v 5) (o<) (g) (v 3) (o>) (c) (v 5) (o<) (f) (v 3) (g)
    ;; proper introduction
    (m3)
    ;; section A (bass)
    [repeat
     2 [repeat 3 (m3)]
     [repeat
      4 (o 1) (v 5) (a+ 16) (v 3) (a+ 16)
      [repeat
       2 (v 4) (o 4) (c 16) (v 2) (c 16) (v 4)
       (o<) (d 16) (v 2) (d 16) (v 5) (o<) (o<) (a+ 32) (v 3) (a+ 32)]
      (v 4) (o 4) (c 16) (o<) (c 16)]
     (m3)
     [repeat
      2 (o 2) (v 5) (c+ 16) (v 3) (c+ 16)
      [repeat
       2 (v 4) (o 4) (c+ 16) (v 2) (c+ 16) (v 4)
       (o<) (d+ 16) (v 2) (d+ 16) (v 5) (o<) (c+ 32) (v 3) (c+ 32)]
      (v 4) (o>) (o>) (c+ 16) (o<) (c+ 16)]]
    [repeat 2 (m3)]
    ;; section B1
    (m13) (m13)
    (r 64) (m15)                        ; solo
    (m13) (m13)
    ;; section B2
    [repeat
     2 (m5) (v 6)
     [repeat 2 (o 3) (f 64) (g) (o>) (d) (f) (g) (o>) (d) (f) (g 32)
             (f 64) (d) (o<) (g) (f) (d) (o<) (g) (f)]]
    (r 2))
  (define (channel-c)
    ;; paganini-esque introduction
    (r 4)
    (r 8) (v 2) (m25)
    ;; fade-in
    (r 1) (r) (r)
    (repeat
     3
     (o 3) (v 2) (d 32) (v 1) (d) (v 2) (c) (v 1) (c) (v 2) (g) (v 1) (g) (v 2)
     (o<) (g) (v 1) (g) (v 2) (o>) (d) (v 1) (d) (v 2) (e) (v 1) (e) (v 2)
     (g) (v 1) (g) (v 2) (o<) (g) (v 1) (g))
    ;; proper introduction
    (repeat 15 (m1))
    ;; section A1 (melody)
    [repeat 2 (m5) (m1)]
    (m6) (m5) (m1) (m4)
    (m1) (m1)
    (o 3) (v 4) (d 32) (v 2) (d) (v 4) (c) (v 2) (c) (v 4) (g) (v 2)
    (g) (v 4) (o<) (g) (v 2) (g)
    (v 7) (repeat 2 (m7) (m8) (v 6))
    (v 5) (m7) (m8) (v 4) (m7)
    ;; section A2 (melody)
    [repeat 2 (m5) (m1)] (m6) (m5) (m1)
    (o 3) (v 6) (c 12) (v 4) (c 12) (v 2) (c 8) (v 6)
    (f 8) (v 2) (f 16) (v 6) (g+ 8) (v 2) (g+ 16) (v 6) (a+ 12)
    (v 4) (a+ 12) (v 2) (a+ 8) (v 6) (g+ 12) (a 64) (a+) (b) (o>)
    (c) (c+) (d) (d+) (e) (f) (f+) (g) (g+)

    (v 6) [repeat 18 (o 3) (c 64) (e 64) (g 64) (o>) (f+ 64) (d 64) (o<) (b 64)]
    (c 64) (e 64) (g 64) (o>) (f+ 64)
    [repeat 18 (o 4) (d 64) (c 64) (o<) (d+ 64) (f 64) (g+ 64) (o>) (g 64)]
    (d 64) (c 64) (o<) (d+ 64) (f 64)
    [repeat 18 (o 3) (a 64) (f+ 64) (d 64) (o>) (a 64) (f+ 64) (d 64)]
    (f+ 64) (d 64) (o>) (a 64)
    (v 7) [repeat 10 (o 4) (g 64) (d 64) (o<) (a+ 64) (g 64) (d 64) (o>) (a+ 64)]
    (g 64) (d 64) (o<) (a+ 64) (g 64) (d 64)
    (v 8) [repeat 7 (o 4) (b 64) (g 64) (d+ 64) (o<) (b 64) (g 64) (d 64)]
    (b 64) (g 64) (d+ 64) (o<) (b 64) (g 32)
    ;; section B1
    [repeat 4 (m10)]
    [repeat
     2
     (o 1) (c 8) (o>) (c 64) (r 48) (o<) (c 16) (r 8) (o>) (c 8)
     (o<) (v 8) [repeat 6 (c 32) (r 32)] (v 7) (g 8)
     (r 8) (o>) (c 64) (r 24) (r 64) (g 8) (o<) (v 8)
     [repeat 6 (g 32) (r 32)] (v 7) (c+ 8) (o>) (c 64) (r 48)
     (o<) (c+ 16)
     (r 8) (o>) (c+ 8) (o<) (v 8)
     [repeat 6 (c+ 32) (r 32)]
     (v 7)
     (g+ 8) (r 16) (o>) (c 64) (r 24) (r 64) (c 64) (r 48)
     (g+ 24) (g 64) (d 64) (o<) (g 16) (a 32) (r 32) (f 32) (r 32)
     (g 16) (o>) (c 16) (g 16)]
    ;; section B2
    (m10) (m10)
    (r 2))
  (define (drum i)
    (case-lambda (()
                  (parameterize ((voice i))
                    (play 1)))
                 ((n)
                  (groove n)
                  (parameterize ((voice i))
                    (play 1)))))
  (define (channel-d)
    (define c (drum (pipe> wowie-kick
                           (volume 8)
                           (dynamic-volume))))
    (define c+ (drum (pipe> arp-lead
                            (volume 4)
                            (dynamic-volume))))
    (define d (drum (pipe> kick-drum
                           (volume 8)
                           (dynamic-volume))))
    (define d+ (drum (pipe> snare-drum
                            (volume 8)
                            (dynamic-volume))))
    (define e (drum (pipe> high-hat
                           (volume 4)
                           (dynamic-volume))))
    (define (m9)
      (d 16) (r) (e) (c+) (d+) (r) (e) (r) (e) (r) (c+) (e) (d+) (r)
      (e) (r) (d) (r) (e) (c+) (d+) (r) (e) (r) (e) (c+)
      (r) (e) (d+) (e) (e) (d)
      (r 8) (e 16) (c+) (d+) (r) (e) (r) (e) (e) (c+) (e) (d+) (r)
      (e) (d) (d+) (d+ 64) (d+ 48)
      (e 16) (c+) (d+) (e) (d+) (d+))
    (define (m11)
      (d 16) (e) (e) (d) (c+) (e) (d+ 64) (d+ 48) (e 16)
      (e) (e) (d) (e) (d) (e))
    (define (m12)
      (d 16) (e) (e) (d) (e) (e) (d+ 64) (d+ 48) (e 16)
      (e) (e) (d) (e) (d+ 64) (d+ 48) (d 16))
    (r 4)
    ;; paganini-esque introduction & fade-in & proper introduction & section A1
    [repeat 58 (r 1)]
    (d+ 64) (d+ 48) (r 16) (d 16) (d+ 16) (e 16) (d+ 64) (d+ 48) (d 16)
    (r 16) (d+ 16) (d+ 16)
    ;; section A2
    [repeat 4 (m9)]
    (d 16) (r) (e) (r) (d) (r) (e) (r) (d+) (r) (e) (r) (e)
    (r) (d) (r) (e) (e) (d+ 64) (d+ 48)
    (r 16) (d) (d+) (e) (d+ 64) (d+ 48) (d 16) (r) (d+) (d+)
    (m9) (m9)
    ;; section B1
    [repeat 8 (m11) (m12)]
    [repeat
     2 [repeat
        3 (d 16) (e 16) (e 16) (d 16) (c+ 16) (e 16) (d+ 64) (d+ 48) (r 16)
        [repeat 6 (c+ 64) (e 48)]]
     (m12)]
    ;; section B2
    [repeat
     2 (m16)
     (m11) (m12)]
    (d+ 16) (c 16) (e 32) (e 32) (c+ 64) (c+ 64) (c+ 64) (c+ 64) (d+ 64) (d+ 48)
    (e 16) (c+ 16) (d+ 64) (d+ 48))
  (define (m1)
    (o 3) (v 4) (d 32) (v 2) (d) (v 4) (c) (v 2) (c) (v 4) (g) (v 2)
    (g) (v 4) (o<) (g) (v 2) (g) (v 4) (o>) (d) (v 2) (d) (v 4) (e)
    (v 2) (e) (v 4) (g) (v 2) (g) (v 4) (o<) (g) (v 2) (g))
  (define (m2)
    (repeat
     7
     (o 3) (v 4) (c 32) (v 2) (c) (v 4) (g) (v 2) (g) (v 4) (o<)
     (g) (v 2) (g) (v 4) (o>) (d) (v 2) (d) (v 4) (e) (v 2) (e) (v 4) (g)
     (v 2) (g) (v 4) (o<) (g) (v 2) (g) (v 4) (o>) (d) (v 2) (d)))
  (define (m3)
    [repeat
     3 (o 2) (v 5) (c 16) (v 3) (c 16)
     [repeat
      2 (v 4) (o 4) (c 16) (v 2) (c 16) (v 4)
      (o<) (d 16) (v 2) (d 16) (v 5) (o<) (c 32) (v 3) (c 32)]
     (v 4) (o>) (o>) (c 16) (o<) (c 16)]
    (o 2) (v 5) (c 16) (v 3) (c 16) (v 5)
    (o>) (o>) (c 16) (v 3) (c 16) (v 5)
    (o<) (d 16) (v 3) (d 16) (v 5) (o<) (c 32) (v 3) (c 32) (v 5) (o>) (o>)
    (c 16) (v 3) (c 16) (v 5) (o<) (o<) (g 16) (v 3) (b 64) (v 5) (o>) (c 48)
    (o<) (c 16) (o>) (d 32) (e 32) (o<) (o<) (b 32) (v 3) (b 32))
  (define (m4)
    (o 4) (v 6) (c 64) (o<) (c 104) (v 4) (c 12) (v 2) (c 8)
    (v 6) (o>) (f 64) (o<) (f 112) (v 2) (f 16) (v 6) (o>) (g+ 64)
    (o<) (g+ 112) (v 2) (g+ 16) (v 6) (o>) (a+ 64) (o<) (a+ 104)
    (v 4) (a+ 12) (v 2) (a+ 8) (v 6) (o>) (g+ 64) (o<) (g+ 104)
    (v 4) (g+ 12) (v 6) (o>) (g 64) (o<) (g 104) (v 4) (g 4))
  (define (m5)
    (o 4) (v 6) (c 64) (o<) (c 104) (v 4) (c 12) (v 2) (c 8) (v 6)
    (o>) (e 64) (o<) (e 112) (v 2) (e 16) (v 6) (o>) (g 64) (o<) (g 112)
    (v 2) (g 16) (v 6) (o>) (b 64) (o<) (b 104) (v 4) (b 12) (v 2)
    (b 8) (v 6) (o>) (c 32) (o>) (d 64) (o<) (d 72) (v 4)
    (d 12) (v 6) (o>) (c 64) (o<) (c 104)
    (v 5) (c 4) (v 4) (c 4) (v 3) (c 2))
  (define (m6)
    (o 4) (v 6) (c 64) (o<) (c 104) (v 4) (c 12) (v 2)
    (c 8) (v 6) (o>) (f 64) (o<) (f 112) (v 2) (f 16) (v 6)
    (o>) (g 64) (o<) (g 104) (v 2) (g 16) (v 6) (o>) (o>)
    (c 64) (o<) (c 104) (v 4) (c 12) (v 2) (c 8) (v 6)
    (o>) (d 64) (o<) (d 104) (v 2) (d 12) (v 6) (e 32)
    (o>) (f 64) (o<) (f 104)
    (v 2) (f 8) (v 6) (d 32) (o>) (e 64) (o<) (e 104) (v 2)
    (e 8) (v 6) (o>) (c 64) (o<) (c 104) (v 4) (c 4)
    (v 3) (c 4) (v 2) (c 4))
  (define (m7)
    [repeat 2 (o 5) (g 64) (e) (d) (c) (o<) (g) (e) (d) (c)])
  (define (m8)
    [repeat 2 (o 4) (g 64) (e) (d) (c) (o<) (g) (e) (d) (c)])
  (define (m10)
    (v 7) (o 1) (c 8) (o>) (c 64) (r 48) (o<) (c 16) (r 8) (o>) (c 8) (r 16)
    (c 64) (r 48) (o<) (c 8) (o>) (c 24) (o<) (b 64) (a 64) (g 8) (r 8) (o>) (c 64)
    (r 24) (r 64) (g 8) (r 16) (c 64) (r 48) (o<) (g 8) (o>) (f 32) (g 24)
    (o<) (c+ 8) (o>) (c 64) (r 48) (o<) (c+ 16) (r 8) (o>) (c+ 8) (r 16)
    (c 64) (r 48) (o<) (c+ 16) (o>)
    (g+ 16) (o<) (c+ 16) (d 64) (e 64) (f 64) (g 64) (g+ 8) (r 16) (o>)
    (c 64) (r 24) (r 64) (c 64) (r 48) (g+ 24) (g 64) (d 64)
    (o<) (g 16) (a 32) (r 32)
    (f 32) (r 32) (g 16) (o>) (c 16) (g 16))
  (define (m13)
    [repeat 2 (v 7) (o 3) (d+ 64) (g 64) (o>) (c 64) (d+ 64) (g 64) (o>)
            (c 64) (d+ 64) (g 32) (d+ 64) (c 64) (o<) (g 64) (d+ 64)
            (c 64) (o<) (g 64) (d+ 64)]
    (r 12)
    [repeat 2 (o 2) (b 64) (o>) (d 64) (g 64) (b 64) (o>) (d 64) (g 64)
            (b 64) (o>) (d 32) (o<) (b 64) (g 64) (d 64) (o<) (b 64)
            (g 64) (d 64) (o<) (b 64)]
    (r 12)
    [repeat 2 (o 3) (f 64) (g+ 64) (o>) (c+ 64) (f 64) (g+ 64) (o>)
            (c+ 64) (f 64) (g+ 32) (f 64) (c+ 64) (o<) (g+ 64) (f 64)
            (c+ 64) (o<) (g+ 64) (f 64)]
    (r 12)
    [repeat 2 (o 3) (c 64) (d+ 64) (g+ 64) (o>) (c 64) (d+ 64) (g+ 64)
            (o>) (c 64) (d+ 32) (c 64) (o<) (g+ 64) (d+ 64) (c 64) (o<)
            (g+ 64) (d+ 64) (c 64)]
    [repeat 2 (o 5) (f 64) (e 64) (c 64) (o<) (a 64) (g 64) (f 64) (e 64)
            (c 64) (o<) (a 64) (g 64) (f 64) (e 64)])
  (define (m14)
    (v 8) (o 4) (c 64) (d+ 64) (o<) (g 64) (a+ 64)
    (v 6) [repeat 7 (o 4) (c 64) (d+ 64) (o<) (g 64) (a+ 64)]
    (v 8) (o 5) (g 64) (a+ 64) (r 32)
    (v 6) [repeat 5 (g 64) (a+ 64) (r 32)]
    (v 8) (o 3) (g 64) (b 64) (o>) (d 64) (f 64)
    (v 6) [repeat 7 (o 3) (g 64) (b 64) (o>) (d 64) (f 64)]
    (v 8) (o 5) (g 64) (b 64) (r 32)
    (v 6) [repeat 5 (g 64) (b 64) (r 32)]
    (v 8) (o 4) (c+ 64) (f 64) (o<) (g+ 64) (o>) (c 64)
    (v 6) [repeat 7 (o 4) (c+ 64) (f 64) (o<) (g+ 64) (o>) (c 64)]
    (v 8) (o 5) (f 64) (g+ 64) (r 32)
    (v 6) [repeat 5 (f 64) (g+ 64) (r 32)]
    (v 8) (o 3) (g+ 64) (o>) (c 64) (d+ 64) (g 64)
    (v 6) [repeat 7 (o 3) (g+ 64) (o>) (c 64) (d+ 64) (g 64)]
    (v 8) (f 64) (e 64) (c 64) (g 64)
    (v 6) [repeat 5 (f 64) (e 64) (c 64) (g 64)])
  (define (m15)
    (v 7) (o 4) (a+ 64) (o>) (c 48) (o<) (g 16)
    (d+ 16) (o<) (g 16) (d+ 16) (f 16) (g 16) (o>)
    (c 32) (r 32) (d+ 16) (c 16) (o<) (g 16) (v 8)
    (d+ 32) (r 32) (c 16) (o<) (g 16) (v 7) (b 16)
    (o>) (g 16) (d 16) (b 48) (o>) (c 64) (c+ 64)
    (d 48) (g 16) (b 16) (o>) (d 16) (g 16) (b 16)
    (g 16) (d 16) (o<) (b 32) (r 32) (f 64) (g 48)
    (f 16) (c+ 16) (o<) (g+ 16) (f 16) (o>) (o>)
    (c 64) (f 48) (c+ 16) (o<) (g+ 16) (f 48)
    (r 64) (c+ 16) (o<) (g+ 32) (r 32) (f 16) (g+ 32)
    (r 32) (c+ 48) (r 64) (f 16) (g+ 32) (r 32) (o>)
    (c 16) (d 64) (d+ 48) (g+ 16) (o>) (c 16) (d+ 16)
    (g 16) (f 16) (c 16) (o<) (g 16) (d+ 64) (f 48)
    (c 16) (o<) (g 16) (f 16) (v 8) (c 32) (d+ 32) (g 32)
    (o>) (c 32) (d+ 32) (g 32) (o>) (c 32) (d+ 32) (g 32)
    (d+ 32) (c 32) (o<) (g 32) (d+ 32) (c 32) (o<) (g 32)
    (d+ 32) (v 7) (c 32) (r 32) (d 32) (r 32) (d+ 32)
    (r 32) (f 32) (r 32) (g 32) (r 32) (b 32) (r 32)
    (v 8) (o<) (g 32) (b 32) (o>) (d 32) (g 32) (b 32)
    (o>) (d 32) (g 32) (b 32) (o>) (d 32) (o<) (b 32)
    (g 32) (d 32) (o<) (b 32) (g 32) (d 32) (o<) (b 32)
    (v 7) (o>) (g 64) (o<) (g 32) (r 64) (o>) (a 64) (o<)
    (a 32) (r 64) (o>) (b 64) (o<) (b 32) (r 64) (o>)
    (o>) (c 64) (o<) (c 32) (r 64) (o>) (d 64) (o<) (d 32)
    (r 64) (o>) (d+ 64) (o<) (d+ 32) (r 64) (v 8) (c+ 32)
    (f 32) (g+ 32) (o>) (c+ 32) (f 32) (g+ 32) (o>) (c+ 32)
    (f 32) (g+ 32) (f 32) (c+ 32) (o<) (g+ 32) (f 32)
    (c+ 32) (o<) (g+ 32) (f 32) (v 7) (c+ 16) (d+ 16)
    (f 32) (r 32) (a+ 16) (g+ 16) (o>) (c+ 16) (f 64)
    (g 48) (d+ 16) (c 32) (r 32) (o<) (g+ 16) (g 16)
    (d+ 16) (c 16) (g 16) (a 32) (o>) (c 32) (d 32)
    (d+ 32) (v 8) (o>) (e 64) (o<) (e 32) (r 64)
    (o>) (f 64) (o<) (f 32) (r 64) (o>) (g 64) (o<)
    (g 32) (r 64) (o>) (c 48))
  (define (m16)
    (d 12) (c 16) (e 8) (d 8) (d+ 64)
    (d+ 24) (r 64) (e 16) (d 16) (e 8) (d 8)
    (c 16) (d 16) (e 8) (d+ 64) (d+ 24) (r 64)
    (e 8) (d 16) (e 16) (d+ 64) (d+ 24) (r 64))
  (define (m25)
    ;; paganini-esque solo (intro take from the movie crossroads)
    ;; Blake, you're mad
    (o 2) (t 90) (c 16) (t 70) (d+)
    (t 56) (g) (o>) (c) (d+) (g) (d+)
    (t 55) (c) (o<) (g) (d+) (c) (o<) (d+) (c)
    (t 80) (d) (f)
    (t 70) (g) (b) (o>)
    (t 60) (d)
    (t 50) (f) (g) (b) (o>) (d) (o<) (b)
    (t 55) (g) (f) (d) (o<) (b)
    (t 60) (g)
    (t 70) (f)
    (t 70) (c) (e)
    (t 65) (g) (a+) (o>)
    (t 56) (c) (e) (g) (a+) (o>) (c) (o<) (a+) (g) (e) (c) (o<) (a+) (g) (e)
    (t 60) (c) (f)
    (t 55) (g+) (o>) (c) (f)
    (t 53) (g+) (o>) (c) (f) (g+)
    (t 55) (f) (c) (o<) (g+) (f) (c) (o<) (g+) (f) (d) (f) (g+) (b) (o>) (d) (f) (g+) (b) (o>) (d) (o<) (b) (g+) (f) (d) (o<) (b) (g+) (f)
    (t 50) (c) (g) (d+) (o>) (c) (o<)
    (t 55) (g) (o>) (d+) (c) (g) (d+) (o>)
    (t 80) (c) (o<) (g) (o>) (d+)
    (t 90) (c) (g) (d+)
    (t 100) (b)
    (t 200) (o>) (c 4)
    (t 40) (r 1)
    ;; let's gooooo!
    (t 60) (c 16) 
    (t 50) (o<) (g 16) 
    (t 48) (d+ 16) (o<) (g 16) (d+ 16) (f 16) 
    (t 44) (g 16) (o>) (c+ 16) (d 16) (o<) (b 16) (g 16) (d+ 16) (d 16) 
    (t 40) (o<) (b 16) (o>) (d 16) (f 16)
    (d+ 16) (g 16) (a+ 16) (o>) (d+ 16) (g 16) (a+ 16) (o>) (d+ 16) (g 16) (a+ 16) (f 16) (d 16) (o<) (a+ 16) (f 16) (d 16) (o<) (a+ 16) (f 16)
    (o>) (o>) (e 16) (c 16) (o<) (g 16) (e 16) (c 16) (o<) (g 16) (e 16) (g 16) (c 16) (f 16) (g+ 16)(o>) (c 16) (f 16) (g+ 16)(o>) (c 16) (f 16)
    (a+ 16) (f 16) (d 16) (o<) (a+ 16) (f 16) (d 16) (o<) (a+ 16) (f 16) (o<) (a+ 16) (o>) (d+ 16) (g 16) (a+ 16) (o>) (d+ 16) (g 16) (a+ 16) (o>) (d+ 16)
    (f 16) (d 16) (o<) (b 16) (g+ 16) (f 16) (d 16) (o<) (b 16) (g+ 16)(o>) (d+ 16) (c 16) (o<) (g 16) (d+ 16) (c 16) (o<) (g 16) (d+ 16) (g 16)
    (o>) (g+ 16) (f 16) (o>) (o>) (c 16) (o<) (g+ 16)(o<) (b 16) (g 16) (o>) (o>) (d 16) (o<) (b 16) (o>) (c 16) (o<) (g 16) (d+ 16) (o<) (g 16) (c 16) (r 8) (r 4)
    (o>) (c 32) (d+ 32) (g 32)(o>) (c 32) (d+ 32) (r 16) (r 8) (o<) (o<) (g 32) (b 32)(o>) (d 32) (g 32) (b 32)(o>) (d 32) (r 16) (o<) (o<) (c 32) (d+ 32) (g 32)(o>) (c 32) (d+ 32) (g 32)
    (o>) (c 32)(o<) (g 32) (d+ 32) (c 32)(o<) (g 32) (d+ 32) (c 32) (r 16) (r 8)
    (o>) (o>) (c 16) (o<) (g 16) (d+ 16) (c 16) (g 16) (d+ 16) (c 16) (o<) (f+ 16) (g 16) (o<) (b 16) (o>) (d 16) (f+ 16) (g 16) (b 16) (o>) (d 16) (f 16)
    (c 32) (d+ 32) (g 32)(o>) (c 32) (d+ 32) (r 16) (r 8)(o<) (o<) (g 32) (b 32)(o>) (d 32) (g 32) (b 32)(o>) (d 32) (r 16)(o<) (o<) (c 32) (d+ 32) (g 32)(o>) (c 32) (d+ 32) (g 32)
    (o>) (c 32)(o<) (g 32) (d+ 32) (c 32)(o<) (g 32) (d+ 32) (c 32) (r 16) (r 8)
    (c 16) (e 16) (g 16) (o>) (c 16) (o<) (d 16) (f 16) (a+ 16) (o>) (f 16) (o<) (g 16) (a+ 16) (o>) (d+ 16) (g 16) (o<) (b 16) (o>) (d 16) (g 16) (b 16)
    (o<) (c 32) (e 32) (g 32)(o>) (c 32) (e 32) (g 32)(o>) (c 32) (e 32) (c 32)(o<) (g 32) (e 32) (c 32)(o<) (g 32) (e 32) (c 32) (e 32) (f 32) (g+ 32)(o>) (c 32) (f 32) (g+ 32)
    (o>) (c 32) (f 32) (g+ 32) (g 32) (g+ 32) (f 32) (c 32)(o<) (g+ 32) (f 32) (c 32)(o<) (g+ 32)
    (o<) (a+ 32)(o>) (d 32) (f 32) (a+ 32)(o>) (d 32) (f 32) (a+ 32)(o>) (d 32)(o<) (a+ 32) (f 32) (d 32)(o<) (a+ 32) (f 32) (d 32)(o<) (a+ 32)(o>) (d 32) (d+ 32) (g 32)
    (a+ 32)(o>) (d+ 32) (g 32) (a+ 32)(o>) (d+ 32) (g 32) (a+ 32) (g 32) (d+ 32)(o<) (a+ 32) (g 32) (d+ 32)(o<) (a+ 32) (g 32)
    (o<) (g+ 32)(o>) (c 32) (d+ 32) (g+ 32)(o>) (c 32) (d+ 32) (g+ 32)(o>) (c 32)(o<) (g+ 32) (d+ 32) (c 32)(o<) (g+ 32) (d+ 32) (c 32)(o<) (g+ 32)(o>) (c 32) (d 32)
    (f+ 32) (a 32)(o>) (d 32) (f+ 32) (a 32)(o>) (d 32) (f+ 32) (a 32) (f+ 32) (d 32)(o<) (a 32) (f+ 32) (d 32)(o<) (a 32) (f+ 32)
    (g+ 16) (f 16) (o>) (o>) (c 16) (o<) (g+ 16)(o<) (b 16) (g 16) (o>) (o>) (d 16) (o<) (b 16) (o>) (c 16) (o<) (g 16) (d+ 16) (o<) (g 16) (c 16) (r 8) (r 4) 
    (o>) (o>) (d+ 16) (c 16) (o<) (g 16) (d+ 16) (c 16) (g 16) (d+ 16) (c 16) (o>) (d 16) (o<) (b 16) (g 16) (d 16) (b 16) (g 16) (d 16) (o<) (b 16)
    (o>) (o>) (d+ 16) (c 16) (o<) (g 16) (d+ 16) (c 16) (g 16) (d+ 16) (c 16) (o>) (d 16) (o<) (b 16) (g 16) (d+ 16) (d 16) (d+ 32) (d 32) (d+ 32) (d 32) (d+ 32) (d 32)
    (o>) (g 16) (e 16) (c 16) (o<) (g 16) (e 16) (c 16) (o<) (a+ 16) (g 16) (o>) (o>) (f 16) (c 16) (o<) (g+ 16) (f 16) (c 16) (o<) (g+ 16) (f 16) (e 16)
    (o>) (o>) (f 16) (d 16) (o<) (a+ 16) (g+ 16) (f 16) (d 16) (o<) (a+ 16) (g+ 16)(o>) (o>) (d+ 16) (o<) (a+ 16) (g 16) (d+ 16) (o<) (a+ 16) (o>) (d+ 16) (o<) (g 16) (a+ 16)
    (o>) (o>) (d 16) (c 16) (o<) (b 16) (g+ 16) (g 16) (f 16) (d+ 16) (d 16) (c 16) (o<) (a+ 16) (g+ 16) (g 16) (f 16) (f+ 16) (g 16) (c 16)
    (g+ 16) (f 16) (o>) (o>) 
    (t 50) (c 16) (o<) (g+ 16)(o<) (b 16) 
    (t 60) (g 16) (o>) (o>) (d 16) (o<) (b 16) (o>) 
    (t 70) (c 16) (o<) (g 16) 
    (t 80) (e 16)(o<) 
    (t 90) (g 16) 
    (t 200) (c 4))
  (define v
    (make-parameter 8))
  (define o
    (make-parameter 1))
  (define (o>)
    (o (+ (o) 1)))
  (define (o<)
    (o (- (o) 1)))
  (define (dynamic-volume carrier)
    (lambda (i) (* (v) (carrier i))))
  (define (dynamic-octave carrier)
    (lambda (i) (carrier (* i (expt 2 (- (o) 1))))))
  (define (t n)
    (tempo (- 256 n)))
  (define (play-note i)
    (case-lambda
      (() (play (index-note-to-scale i scale)))
      ((n)
       (groove (/ 8 n))
       (play (index-note-to-scale i scale)))))
  (define r
    (let ((last-rest 1))
      (case-lambda
        (() (parameterize ((groove last-rest))
              (play #f)))
        ((n) (set! last-rest (/ 8 n))
         (parameterize ((groove last-rest))
           (play #f))))))
  (define c (play-note 0))
  (define c+ (play-note 1))
  (define d (play-note 2))
  (define d+ (play-note 3))
  (define e (play-note 4))
  (define f (play-note 5))
  (define f+ (play-note 6))
  (define g (play-note 7))
  (define g+ (play-note 8))
  (define a (play-note 9))
  (define a+ (play-note 10))
  (define b (play-note 11))
  (define scale (rank-2-temperament 3 2 12))
  (root 100)
  (voice (pipe> triangle
                (adsr 1/3 0 1 1/3)
                (dynamic-octave)
                (dynamic-volume)))
  (render-tail 1/3)
  (rewind) (groove 1) (t 60) (o 3) (channel-a)
  (rewind) (groove 1) (t 60) (o 3) (channel-b)
  (rewind) (groove 1) (t 60) (o 3) (channel-c)
  (rewind) (groove 1) (t 60) (o 3) (channel-d))

(define-song "wings not right"
  (define PAD
    (pipe> triangle
           (adsr 1/2 0 1 1/2)
           (volume 1/4)
           (am (pipe> sin (hz 9)
                      (volume 1/2)
                      (offset 1/2))
               (pipe> sin (hz 27)
                      (volume 1/2)
                      (offset 1/2)))
           (fm (pipe> sin (detune 1/2)
                      (volume 1/1024)
                      (offset 1)))))
  (define STABS
    (pipe> sin
           (add (pipe> (harmonics 1 2 3 4 6 8 9 12 16 18)
                       (detune 1/2)
                       (volume 1/2)))
           (adsr 1/2 1/2 1/16 1/4)
           (am (pipe> sin
                      (adsr 1 0 1 1/4)
                      (hz 4)
                      (offset 1)))
           (fm (pipe> sin
                      (adsr 0 1/2 1/256 1/4)
                      (detune 3)
                      (volume 1/256)
                      (offset 1)))
           (volume 1/6)))
  (define BASS
    (pipe> frog-bass
           (add (pipe> (harmonics 1/2 1/4)
                       (adsr 1/32 31/32 0 0)))
           (volume 3)))
  (define LEAD
    (pipe> arp-lead
           (detune 2)
           (adsr 1/2 1/2 0 0)
           (volume 2)))
  (define KICK
    (pipe> kick-drum3
           (volume 4)))
  (define SNARE
    (pipe> snare-drum3
           (volume 3)))
  (define HAT
    (pipe> high-hat
           (volume 2)))
  (define scale
    (rank-2-temperament 3 2 7))
  (define (change-mode! n)
    (set! scale (rotate-scale n scale)))
  (define (play+ . chords)
    (apply (index-scale play scale) chords))
  (define (triad n)
    (map (lambda (n) (modulo n 16))
         (list (* 3 n)
               (+ 2 8 (* 3 n))
               (+ 4 (* 3 n)))))
  (define (tetrad n)
    (map (lambda (n) (modulo n 16))
         (list n
               (+ 2 8 (* 3 n))
               (+ 4 (* 3 n))
               (+ 6 (* 3 n)))))
  (define (rotate lst)
    (append (cdr lst) (list (car lst))))
  (root 110)
  (tempo 124)
  (render-tail 1/2)
  (display "LEAD\n")
  (do ((i 0 (+ 1 i))
       (m (list #f 0 2 5
                4 #f 5 4
                3 #f 5 4)
          (pipe>> m
                  (map (lambda (m) (and (number? m)
                                        (modulo (+ 3 m) 9))))
                  (rotate))
          ))
      ((= i 8))
    (groove 6 3 2 1
            6 3 2 1
            4 2 1 1)
    (voice LEAD)
    (for-each play+ m)
    (change-mode! 12))
  (display "CHORDS & BASS\n")
  (rewind)
  (do ((i 0 (+ i 1)))
      ((= i 64))
    (voice STABS)
    (cond ((even? i)
           (groove 3 1)
           (play+ #f (tetrad i)))
          ((zero? (modulo i 7))
           (groove 1/2)
           (play+ #f #f
                  #f (triad i)
                  #f (tetrad i)
                  #f (triad i)))
          (else
           (groove 1)
           (play+ #f (tetrad i)
                  #f (tetrad i))))
    (unless (> (modulo i 8) 5)
      (voice PAD)
      (rewind 4)
      (groove 2)
      (play #f (modulo i 3)
            #f (modulo (* 5 i) 3))
      (rewind 4))
    (voice BASS)
    (rewind 4)
    (let ((t (map (lambda (i) (modulo i 8))
                  (triad i))))
      (cond ((zero? (modulo i 7))
             (groove 3/2 1/2 1/2 1/2 1/2 1/2)
             (play+ (car t) (cadr t)
                    #f (caddr t)
                    #f (caddr t)))
            (else
             (groove 2 1/2 1/2 1/2 1/2)
             (play+ (car t)
                    #f (caddr t)
                    #f (caddr t)))))
    (change-mode! 4))
  (rewind)
  (display "RHYTHM\n")
  (do ((i 0 (+ i 1)))
      ((= i 32))
    (voice KICK)
    (groove 1/2)
    (play 1 #f #f #f
          1 #f 1 #f
          1 #f #f #f
          1 #f 1 #f)
    (rewind 8)
    (voice SNARE)
    (cond ((zero? (modulo (+ 1 i) 3))
           (groove 1 1/2 1/2 3/2 1/6 1/6 1/6)
           (play #f 1 1 #f 1 1 1
                 #f 1 1 #f #f 1 1))
          ((even? i)
           (groove 1/2)
           (play 1 1 #f #f #f  1 #f #f
                 1 1 #f  1  1 #f #f #f))
          (else
           (groove 1 1/2 1/2)
           (play #f 1 #f #f 1 #f
                 #f 1 #f #f 1 #f)))
    (rewind 8)
    (voice HAT)
    (groove 1/2 1/2 1/2 1 1 1/2
            1/2 1/2 1/2 1 1 1/6 1/6 1/6)
    (play 1 1 1 1 1 1
          1 1 1 1 1 1 1 1)))

(define-song "wild things"
  (define (f k i) (* 3 (+ 1 k) (expt 2 i)))
  (define scale (rank-2-temperament 3 2 7))
  (define play+ (index-scale play scale))
  (define (chord i)
    (map (lambda (n) (modulo n (* 2 (vector-length scale))))
         (list i (+ 2 i) (+ 4 i) (+ 6 i))))
  (root 192)
  (tempo 130)
  (do ((k 0 (+ 1 k)))
      ((= k 8))
    (do ((i 0 (+ i 1))
         (c (chord 0)
            (chord (f k i))))
        ((= i 8))
      (format "LOOP: ~a~%" i)
      (voice greensleeves-chords)
      (groove 1/2)
      (play+ #f c #f c
             #f c #f c)
      (rewind 4)
      (groove 1)
      (pipe>> c
              (sort (if (even? i) < >))
              (map (lambda (n) (+ 2 n)))
              (for-each play+))
      (voice (pipe> triangle-pad
                    (fm (pipe> sin
                               (hz 3)
                               (volume 1)
                               (offset 1/2)))
                    (detune 1/2)))
      (rewind 4)
      (groove 4)
      (play+ (list (car c) (caddr c)))
      (when (zero? (modulo (+ k i) 3))
        (let ((n (f (* 16 k) i)))
          (modulate (index-note-to-scale n scale))
          (while (>= (root) 200)
            (modulate 1/2))
          (while (<= (root) 100)
            (modulate 2/1))
          (set! scale (rotate-scale
                       (modulo n (vector-length scale))
                       scale)))))))

(define-song "ok-flop"
  (define (ok f l o p)
    (+ (expt 2 f)
       (expt 3 l)
       (expt 5 o)
       (expt 7 p)))
  (define (flop o k)
    (+ (expt o 3)
       (expt k 5)))
  (define scale (rank-2-temperament 3 2 5))
  (define (play+ . chords)
    (apply (index-scale play scale) chords))
  (define (rescale i)
    (let ((n (index-note-to-scale (modulo (flop i 2) (vector-length scale)) scale)))
      (modulate n)
      (while (>= (root) 330)
        (modulate 1/2))
      (while (<= (root) 100)
        (modulate 2/1))
      (set! scale (rotate-scale i scale))))
  (root 110)
  (tempo 60)
  (voice bass-drum)
  (groove 1/8 7/8)
  (repeat (* 4 16) (play 1 #f))
  (rewind)
  (voice (pipe> snare-drum3
                (volume 3)))
  (groove 1/2 1/8 3/8)
  (repeat (* 4 16) (play #f 1 #f))
  (rewind)
  (voice (pipe> pluck-bass
                (detune 4)
                (volume 4)))
  (groove 1/4)
  (repeat
   4
   (repeat
    3
    (play+ -1 0 #f #f #f
           -1 0 #f #f #f
           -1 0 #f (modulo (tape-head) 4) #f
           -1 0 #f 1 #f)
    (rescale 2))
   (play+ -1 0 #f #f))
  (rewind)
  (rescale -24)
  (voice (pipe> triangle-pad
                (volume 1/4)))
  (groove 2 2
          1/2 1/2
          1/2 1/2
          1/2 1/2
          1/2 1/2)
  (repeat
   4
   (play+ '(3 5 7)
          '(2 4 6)
          '(2 3) (modulo (tape-head) 10)
          '(2 3) (modulo (tape-head) 10)
          '(2 3) (modulo (tape-head) 10)
          '(2 3 5) #f)
   (rescale 2))
  (groove 2 2 2 1)
  (repeat
   4
   (play+ '(3 5 6)
          '(4 6 7)
          '(3 5 6)
          (modulo (tape-head) 10))
   (rescale 2))
  (groove 1/2)
  (play+ '(2 3) (modulo (tape-head) 10)
         '(2 3) (modulo (tape-head) 10)
         '(2 3) (modulo (tape-head) 10)
         '(2 3 4) #f))

(define-song "theorems"
  (define (f i n) (+ n 3 (exact (floor (/ i 3)))))
  (define (g i n) (+ n (expt i 3)))
  (root 110)
  (tempo 60)
  (voice greensleeves-chords)
  (groove 4)
  (do ((i 0 (+ i 1))
       (m 5 (g i m))
       (scale (rank-2-temperament 3 2 7)
              (let ((n (modulo (g 7 7) 8)))
                (modulate (index-note-to-scale n scale))
                (when (>= (root) 220)
                  (modulate 1/2))
                (when (<= (root) 55)
                  (modulate 2/1))
                (rotate-scale n scale)))
       (chord '(0 4 6 9)
              (map (lambda (n)
                     (modulo (f i n) 16))
                   chord)))
      ((>= i 32))
    (let ((play (index-scale play scale)))
      (play chord)
      (rewind 2)
      (play (modulo m 16))
      (rewind 7/2)
      (pipe>> chord
              (map (lambda (i) (modulo (g m i) 16)))
              (sort (if (even? i) < >))
              (for-each play))
      (rewind (* 7/2 (length chord))))))

(define-song "ambience-2024-09-27"
  (define (next-modulation n)
    (+ n (expt 3 n) (expt n 3)))
  (define (chord i)
    (map (lambda (i) (modulo (* 3 i) 12))
         (factorize (+ 3 (expt i 5) (* 35 i) (modulo i 7))
                    '(2 3 5 7))))
  (root 110)
  (tempo 60)
  (voice triangle-pad)
  (groove 2)
  (do ((bar 0 (+ 1 bar))
       (scale (rank-2-temperament 3 2 7)
              (let* ((i (modulo (next-modulation bar) 8))
                     (m (index-note-to-scale i scale)))
                (modulate m)
                (when (>= (root) 330)
                  (modulate 1/2))
                (when (<= (root) 55)
                  (modulate 2))
                (rotate-scale i scale))))
      ((>= bar 8))
    (pipe>> (iota 8)
            (map (lambda (i) (chord (+ (expt i 3)
                                       (expt bar 3)))))
            (for-each (index-scale play scale)))
    (rewind 8)))

(define-song "fractran"
  (define (f k n)
    (+ k (expt n 15)))
  (root 110)
  (tempo 45)
  (voice greensleeves-chords)
  (groove 1/4 1/4 2 1/4
          1/2 1/4 1/4
          2 1/2 1/4
          1/4 2 1/4
          1/2 1 3)
  (let ((scale (rank-2-temperament 3 2 5)))
    (pipe>>
     (iota 32)
     (for-each
      (lambda (k)
        (rewind 4)
        (do ((n 0 (+ 1 n)))
            ((>= n 4))
          (pipe>> (iota 4)
                  (map (lambda (e)
                         (+ e (* 112332 k) 1)))
                  (map (lambda (e)
                         (index-note-to-scale
                          (modulo (f (+ n k) e) 16)
                          scale)))
                  (for-each (lambda (i)
                              (pretty-print i)
                              (play i))))
          (modulate (snap-note-to-scale 4/3 scale))
          (set! scale (rotate-scale 2 scale))
          (when (>= (root) 330)
            (modulate 1/2))))))))

(define-song "hexagons"
  (let* ((step 16/15)
         (snap (snap-to-chroma step))
         (play (snap-to-scale
                play 11-limit-thing)))
    (root 110)
    (tempo 90)
    (voice (pipe> shiny-sin
                  (distort
                   (lambda (s) (expt s 2))
                   (lambda (s) (- s 1))
                   (lambda (s) (expt s 3))
                   (lambda (s) (max -1 (min s 1))))))
    (groove 40)
    (play #f #f)
    (groove 4 2 2
            4 1 1 2
            4 4/3 4/3 4/3
            4 1 1 1 1)
    (play #f 2 9/4
          2 9/4 2 #f
          2 9/4 2 #f
          9/4 8/3 3 8/3 15/8
;;;
          2 8/5 3/2
          8/5 9/5 #f 2
          9/4 7/3 2 9/4
          7/3 2 9/4 7/3 #f)
    (rewind)
    (render-tail 1)
    (do ((chords (list (list 1 3/2 15/8 5/2)
                       (list 1 3/2 16/9 5/2)
                       (list 1 4/3 5/3 9/4)
                       (list 15/16 5/4 25/16 9/4)
                       (list 5/6 5/4 3/2 2/1))
                 (map (lambda (chord)
                        (map (lambda (note)
                               (* 1/2 (interval-reduce (* note 9/4) (snap 3))))
                             chord))
                      chords))
         (i 0 (+ 1 i)))
        ((= i 4))
      (format #t "Step: ~a~%" i)
      (for-each (lambda (chord)
                  (voice (volume kick-drum3 16))
                  (groove 1/4)
                  (repeat 8 (play 1 #f #f #f))
                  (rewind 8)
                  (voice (pipe> frog-bass
                                (distort
                                 (lambda (s) (* (sin s) (expt s 5)))
                                 (lambda (s) (max -1 (min s 1))))
                                (volume 8)))
                  (groove 1/2)
                  (play (car chord) (/ (car chord) step)
                        (car chord) #f
                        (car chord) (cadr chord)
                        (car chord) #f)
                  (play (car chord) (* (car chord) step)
                        (car chord) #f
                        (car chord) (cadr chord)
                        (car chord) (/ (car chord) step))
                  (rewind 8)
                  (voice (pipe> wobble-strings
                                (distort
                                 (lambda (s) (expt s 3))
                                 (lambda (s) (max -1 (min s 1))))))
                  (groove 1 1 1/2 1/4 1/4 1)
                  (play chord #f chord chord #f chord
                        chord #f chord chord #f #f))
                chords))))

(define-song "heath-j"
  ;; key of Am
  (let* ((play (snap-to-scale play (edi 2 12)))
         (chords:a
          (lambda ()
            (groove 1/2)
            ;; Am7 E7/B
            (play '(1 6/5 3/2 9/5)
                  '(9/8 3/2 15/8 27/10))
            ;; Am7/C E7/B
            (play '(6/5 3/2 9/5 2/1)
                  '(9/8 3/2 15/8 27/10))
            ;; Am7 E7/B
            (play '(1 6/5 3/2 9/5)
                  '(9/8 3/2 15/8 27/10))
            ;; Am7/C A7
            (play '(6/5 3/2 9/5 2/1)
                  '(1 5/4 3/2 9/5))
            ;; Dm7 A7/E
            (play '(4/3 8/5 2/1 12/5)
                  '(3/2 9/5 2/1 5/2))
            ;; Dm7/F A7/E
            (play '(8/5 2/1 12/5 8/3)
                  '(3/2 9/5 2/1 5/2))
            ;; Dm7 A7/E
            (play '(4/3 8/5 2/1 12/5)
                  '(3/2 9/5 2/1 5/2))
            ;; Dm6
            (play '(4/3 8/5 2/1 32/15)
                  '(4/3 8/5 2/1 32/15))
            ;; Bm7 E7
            (play '(9/8 27/20 27/16 81/40)
                  '(3/2 15/8 9/4 27/10))
            ;; Cm7 F7
            (play '(6/5 36/25 9/5 54/25)
                  '(8/5 2/1 12/5 72/25))
            ;; Bm7
            (play '(9/8 27/20 27/16 81/40)
                  '(9/8 27/20 27/16 81/40))
            ;; E7 E+7b9
            (play '(3/2 15/8 9/4 27/10)
                  '(3/2 15/8 75/32 27/10 16/5))))
         (chords:b
          (lambda ()
            (groove 1/2)
            ;; Am7 F#m7b6
            (play '(1 6/5 3/2 9/5)
                  '(5/3 2/1 5/2 8/3))
            ;; F7 E+7
            (play '(8/5 2/1 12/5 72/25)
                  '(3/2 15/8 75/32 27/10))
            ;; Am6/9
            (play '(1 6/5 3/2 5/3 9/5 9/4)
                  '(1 6/5 3/2 5/3 9/5 9/4))
            ;; E+7
            (play '(3/2 15/8 75/32 27/10)
                  '(3/2 15/8 75/32 27/10))))
         (chords:c
          (lambda ()
            (groove 1/2)
            ;; Am7
            (play '(1 6/5 3/2 9/5)
                  '(1 6/5 3/2 9/5))
            ;; F7 E7
            (play '(8/5 2/1 12/5 72/25)
                  '(3/2 15/8 9/4 27/10))
            ;; Am7
            (play '(1 6/5 3/2 9/5)
                  '(1 6/5 3/2 9/5))
            ;; Bm7b5 E7
            (play '(9/8 27/20 63/40 81/40)
                  '(3/2 15/8 9/4 27/10))))
         (lead:a
          (lambda ()
            ;; line 1
            (groove 1/8 1/4 1/8 1/8 1/4 1/8)
            (play 1 3/4 1 9/8 3/4 9/8)
            (groove 1/4 1/8 1/8 1/2)
            (play 6/5 3/2 9/8 #f)
            (groove 1/4 1/4 1/8 1/8 1/8 1/8)
            (play 1 1 9/8 1 9/8 6/5)
            (groove 1/8 1/8 1/8 1/8 1/4 1/8 1/8)
            (play #f 3/4 8/5 5/6 #f #f 1)
            ;; line 2
            (groove 1/8 1/4 1/8 1/8 1/4 1/8)
            (play 4/3 1 4/3 3/2 1 3/2)
            (groove 1/4 1/8 1/8 1/2)
            (play 8/5 2/1 3/2 #f)
            (groove 1/4 1/4 1/12 1/12 1/12 1/8 1/8)
            (play 4/3 4/3 2/1 8/5 3/2 4/3 3/2)
            (groove 1/8 1/8 1/8 1/8 1/2)
            (play 8/5 1 16/15 9/8 #f)
            ;; line 3
            (groove 1/8 1/8 1/8 1/8 5/8) ;; tied to next bar
            (play 3/2 3/2 4/3 5/4 9/8 8/5)
            (groove 1/8 1/8 1/8 1/8 1/8)
            (play 7/5 #f 4/3 6/5 3/2)
            (groove 1/8 1/4 1/8 1/8 1/8 1/8 1/8)
            (play #f 4/3 5/4 9/8 4/5 1 4/5)
            (groove 1/8 1/8 1/8 1/8 1/8 3/8)
            (play 15/16 9/8 3/4 2/3 #f 6/5)))
         (lead:b
          (lambda ()
            (groove 1/4 1/4 1/8 1/8 1/8 1/8)
            (play 1 #f 1 9/10 3/4 2/3)
            (groove 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8)
            (play 3/5 1/2 3/5 9/10 #f 3/4 #f 7/10)
            (groove 1)
            (play 3/4)
            (groove 1/2 1/4 1/8 1/8)
            (play #f #f #f 2/3)))
         (lead:c
          (lambda ()
            (groove 1/4 1/4 1/8 1/8 1/8 1/4) ;; tied to next bar
            (play 1 #f #f 3/4 1 7/5)
            (groove 5/8 1/8 1/8)
            (play 4/3 6/5 4/3)
            (groove 1/8 7/8)
            (play 6/5 1))))
    (root 440)
    (tempo 45)
    (voice (pipe> thunk-pluck
                  (distort
                   (lambda (s)
                     (* (+ (* s (sin s)) (expt s 3)) 3))
                   (lambda (s)
                     (max -1 (min s 1))))
                  (volume 1/2)
                  (detune 1/2)))
    (render-tail 1)
    (chords:a)
    (chords:b)
    (chords:a)
    (chords:c)
    (rewind)
    (voice (pipe> pluck-bass
                  (detune 2)
                  (distort
                   (lambda (s)
                     (expt (* s 1337) 33))
                   (lambda (s)
                     (max -1 (min s 1))))
                  (volume 1/8)))
    (lead:a)
    (lead:b)
    (lead:a)
    (lead:c)))

(define-song "crystal-shards"
  ;; the crystal shard is 16/15
  ;; guitar is tuned by harmonics to 3/4 1/1 3/2 15/8 5/2 3/1
  ;; each fret is one shard
  (root 220)
  (tempo 60)
  (let* ((strings '(3/4 1/1 3/2 15/8 5/2 3/1))
         (guitar (lambda chords
                   (for-each (lambda (chord)
                               (play
                                (map *
                                     (map (lambda (i) (expt 16/15 i)) chord)
                                     strings)))
                             chords))))
    (voice greensleeves-chords)
    (guitar '(0 0 0 0 0 0)
            '(0 0 0 1 0 0)
            '(0 2 2 1 1 0)
            '(0 2 0 0 1 0)
            '(0 0 0 1 0 0))))

(define-song "cat-song"
  (root 200)
  (tempo 180)
  (let* ((chords (lambda (play)
                   (voice (pipe>
                           greensleeves-chords
                           (volume 1/3)
                           (detune 1/2)))
                   (parameterize ((render-tail 3))
                     (groove 1/2 1/6 1/6 1/6)
                     (play '(2 3) #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           '(2 3) #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           '(2 3) #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           #f #f '(2 5/2 3) #f
                           '(2 3) #f '(8/3 3 10/3) #f
                           #f #f '(8/3 3 10/3) #f
                           #f #f '(8/3 3 10/3) #f
                           #f #f '(8/3 3 10/3) #f))))
         (bass (lambda (play)
                 (voice (add ducked-bass
                             bass-drum))
                 (groove 1)
                 (play 1 #f 1 #f 1 #f 1 #f
                       1 #f 1 #f 1 #f 1 #f)))
         (perc (lambda (play)
                 (voice (pipe>
                         wowie-hat
                         (volume 1/3)))
                 (groove 2)
                 (repeat 4 (play #f 8))
                 (rewind 16)
                 (voice (pipe>
                         snare2
                         (volume 2/3)))
                 (groove 1/2)
                 (repeat 8 (play #f #f 1 #f))))
         (arp! (lambda (play i)
                 (groove 1/3)
                 (voice (pipe> arp-lead
                               (detune 1/4)
                               (volume 3/2)))
                 (let ((m (* 1/2 (quick-reduce (expt 81/80 i) 25/24))))
                   (repeat 10 (play (* m 10/7) #f (* m 3/2)))
                   (repeat 2 (play (* m 3/2) (* m 5/3) #f))
                   (repeat 2 (play (* m 5/3) #f (* m 2/1)))
                   (repeat 2 (play (* m 3/2) (* m 5/3) #f)))
                 (rewind 16)
                 (groove 1)
                 (repeat 8 (play #f 2)))))
    (do ((i 0 (+ i 1))
         (scale (harmonic-series-segment 12 24)))
        ((= i 32))
      (format #t "rendering ~a%~%" (inexact (* 100 (/ i 32))))
      (chords (snap-to-scale play scale))
      (rewind 16)
      (bass play)
      (rewind 16)
      (perc play)
      (rewind 16)
      (arp! (snap-to-scale play scale) i))))

(define-song "insane15"
  (let* ((scale (vector-map
                 (snap-to-chroma (expt 2 1/15))
                 5-limit-major))
         (play (reduce-intervals (snap-to-scale play scale) 2)))
    (voice greensleeves-chords)
    (groove 2)
    (root 200)
    (do ((i 0 (add1 i))
         (r 1 (* r 4/3)))
        ((= i 32))
      (play (list r (* 3/2 r) (* 9/4 r))
            (list r (* 3/2 r) (* 9/4 r))
            (list r (* 3/2 r) (* 9/4 r))
            (list r (* 3/2 r) (* 9/4 r))))))

(define-song "share"
  (let* ((scale (merge-scale-offset
                 3/2 (mos:oneirotonic 8/3)))
         (repeats 64)
         (play (pipe> play
                      (snap-to-scale scale))))
    (root 200)
    (tempo 145)
    (render-tail 2)
    (let ((kk (volume kick-drum2 2))
          (hh (volume high-hat 2))
          (s1 (volume rhythm-snare 4))
          (s2 (volume snare-drum3 5/2)))
      (groove 2/3 1/3 2/3 1/3)
      (do ((i 0 (add1 i)))
          ((= i repeats))
        (rhythm play
                kk #f s1 hh
                kk hh s2 hh
                kk #f s1 hh)
        (if (= 13 (mod i 16))
            (rhythm play s2 s2 s2 s2)
            (rhythm play kk hh s2 hh))))
    (rewind)
    (do ((i 0 (add1 i))
         (r 1 (interval-reduce (* r (add1 (expt 4/3 (mod i 4)))) 3/2)))
        ((= i repeats))
      (voice (pipe> soft-chords
                    (adsr 1/2 1/2 1/2 3)
                    (volume 4)))
      (groove 8)
      (play (list (* r 3/2)
                  (* r 9/4)
                  (* r 5/2)
                  (* r 3/1)))
      (rewind 8)
      (voice (pipe> greensleeves-chords
                    (volume 1/8)))
      (cond ((= 7 (mod i 8))
             (groove 1/2 1/2
                     1/2 1
                     1/2 1/2
                     1/2 1
                     1/2 1
                     1/2 1))
            ((= 10 (mod i 12))
             (groove 1/2 1
                     1/2 1/2
                     1/2 1/2
                     1/2 1
                     1/2 1
                     1/2 1))
            ((= 10 (mod i 13))
             (groove 1/2 1/2
                     1/2 1/2
                     1/2 1
                     1/2 1
                     1/2 1
                     1/2 1))
            (else
             (groove 1/2 1
                     1/2 1
                     1/2 1/2
                     1/2 1
                     1/2 1/2
                     1/2 1)))
      (cond ((= 6 (mod i 7))
             (play #f (list r (* r 3/2) (* r 9/4))
                   #f (list r (* r 3/2) (* r 5/2))
                   #f (list r (* r 3/2) (* r 8/3)))
             (play #f (list r (* r 5/2) (* r 3/1))
                   #f (list r (* r 9/4) (* r 8/3))
                   #f (list r (* r 16/9) (* r 8/3))))
            ((= 5 (mod i 9))
             (play #f (list r (* r 3/2) (* r 8/3))
                   #f (list r (* r 3/2) (* r 5/2))
                   #f (list r (* r 3/2) (* r 9/4)))
             (play #f (list r (* r 4/3) (* r 9/4))
                   #f (list r (* r 4/3) (* r 2/1))
                   #f (list r (* r 4/3) (* r 16/9))))
            (else
             (play #f (list r (* r 3/2) (* r 9/4))
                   #f (list r (* r 3/2) (* r 2/1))
                   #f (list r (* r 3/2) (* r 9/5)))
             (play #f (list r (* r 3/2) (* r 2/1))
                   #f (list r (* r 3/2) (* r 9/4))
                   #f (list r (* r 3/2) (* r 2/1)))))
      (rewind 8)
      (voice (pipe> ducked-bass
                    (volume 5/4)))
      (groove 1/2 1
              1/2 1
              1/2 1/2)
      (play #f r
            #f r
            #f r)
      (play #f r
            #f r
            #f (* 2/3 r)))))

(define-song "disappear"
  (let* ((chord (lambda (root . intervals)
                  (cons root (map (lambda (i) (* root i))
                                  intervals))))
         (scale (rank-2-temperament 3 2 7))
         (play (pipe> play
                      (reduce-intervals 2)
                      (index-scale scale))))
    (root 276)
    (tempo 333)
    (voice (pipe> wobble-strings
                  (volume 1/4)
                  (detune 1/2)))
    (groove 10 2 2 10
            6 6 6 6
            10 2 2 10
            8 4 8 4)
    (do ((i 1 (add1 i)))
        ((> i 128))
      (do ((j 0 (add1 j)))
          ((> j (mod (mod i 13) 4)))
        (format #t "chords ~a%...~%"
                (exact (round (* 100 (/ i 128)))))
        (play (list (+ j i) (* (+ j i) 2) (* (+ j i) 3))
              #f
              (list (* (+ j i) 4) (* (+ j i) 5) (* (+ j i) 6))
              #f)))
    (rewind)
    (voice (pipe> fm-triangle-bass))
    (render-tail 2)
    (groove 2 1 2 2
            3 2 3 2
            2 1 2 2)
    (do ((i 1 (add1 i)))
        ((> i 128))
      (format #t "bass ~a%...~%"
              (exact (round (* 100 (/ i 128)))))
      (do ((repeat 0 (add1 repeat)))
          ((> repeat (mod (mod i 13) 4)))
        (do ((j 0 (add1 j)))
            ((= j 12))
          (play (if (odd? i)
                    (* i (- (vector-length scale) j))
                    (* i j)))))
      (newline))
    (let ((end-of-tape (tape-head))
          (kk (volume kick-drum 6))
          (sn (volume snare-drum3 3)))
      (rewind)
      (while (< (tape-head) end-of-tape)
        (format #t "drums ~a%...~%"
                (exact (floor (* 100 (inexact (/ (tape-head) end-of-tape))))))
        (groove 3/2 1/2 1 1)
        ;; maybe a longer rhythm, but this sounds ok
        (rhythm play kk sn kk sn)))))

(define-song "stood-up-too-fast"
  (let* ((scale '#(1 9/8 11/9 9/7 4/3 3/2))
         (play (snap-to-scale play scale))
         (comma 100/99)
         (stack-commas
          (lambda (instr)
            (add instr
                 (pipe> instr (detune comma) (volume 1/2))
                 (pipe> instr (detune (expt comma 2)) (volume 1/4))
                 (pipe> instr (detune (expt comma 3)) (volume 1/8))))))
    (root 200)
    (voice bell)
    (render-tail 3)
    (groove 2 1 3)
    (do ((i 0 (+ i 1))
         (chords '((1 9/7 5/3)
                   (1 11/9 3/2)
                   (1 9/8 4/3))
                 (map (lambda (chord)
                        (map (lambda (note)
                               (* 2/3 (quick-reduce (* note 4/3) 2)))
                             chord))
                      chords)))
        ((= i 16))
      (for-each play chords))))

(let* ((scale (edi 3/2 8))
       (play (snap-to-scale play scale))
       (comma (/ 5/3 (snap-note-to-scale 5/3 scale)))
       (stack-commas
        (lambda (instr)
          (add instr
               (detune instr comma)
               (detune instr (expt comma 2))
               (detune instr (expt comma 3))))))
  (define-song "sipping-soup"
    (format #t "comma: ~a~%" (cents comma))
    (root 200)
    (tempo 120)
    (format #t "rhythm~%")
    (groove 4/5)
    (let ((kk (volume kick-drum 4))
          (sn (volume snare-drum 4)))
      (do ((i 0 (1+ i)))
          ((= i 8))
        (rhythm play
                kk #f sn
                kk #f #f
                kk sn #f
                kk #f sn
                kk #f #f
                #f kk sn kk #f)))
    (rewind 128)
    (format #t "bass~%")
    (voice (pipe> (stack-commas frog-bass)
                  (detune 1/2)
                  (volume 4)))
    (do ((i 0 (1+ i)))
        ((= i 8))
      (groove 8/5 4/5)
      (play 1 1
            1 #f
            1 3/4
            1 #f
            1 #f)
      (groove 4/5)
      (play #f 1 #f 1 2))
    (rewind 128)
    (render-tail 3)
    (voice (stack-commas
            (pipe>
             wobble-strings
             (detune 1/2)
             (volume 1/16))))
    (groove 128/5)
    (format #t "strings~%")
    (play '(1 3/2 9/4)
          '(9/8 3/2 8/3)
          '(1 4/3 15/8)
          '(4/3 5/3 3/1)
          '(5/3 15/8 9/4))
    (rewind 128)
    (format #t "bells~%")
    (do ((i 0 (1+ i)))
        ((= i 4))
      (voice (stack-commas bell))
      (groove 1 1 2 4)
      (play #f '(5/4 3/2) '(5/4 5/3) '(4/3 5/3)
            #f '(4/3 5/3) '(5/4 5/3) '(4/3 5/3)
            #f '(5/4 3/2) '(4/3 5/3) '(5/3 15/8)
            9/4 '(3/2 5/3) '(3/2 15/8) '(15/8 9/4)))
    (rewind 128)
    (format #t "more bells~%")
    (do ((i 0 (1+ i)))
        ((= i 2))
      (groove 16)
      (voice (stack-commas bell))
      (play '(5/6 1/1)
            '(5/6 5/4)
            '(5/6 9/8)
            '(15/16 9/8)))
    (format #t "finale~%")
    (rewind 128)
    (groove 2)
    (voice (stack-commas bell))
    (do ((i 0 (1+ i)))
        ((= i 64))
      (play 1))
    (groove 16)
    (play '(1 3/2 9/4 5/2))))

(define-song "lil-frog-man"
  (root 70)
  (tempo 20)
  (let* ((time 36)
         (a 9)
         (o 3)
         (s 27)
         (divisions 45)
         (scale '#(1 8/7 9/7 10/7))
         (play (pipe> (lambda notes-or-chords
                        (pretty-print notes-or-chords)
                        (apply play notes-or-chords))
                      (reduce-intervals 4)
                      (snap-to-scale scale))))
    (do ((i 1 (1+ i)))
        ((= i divisions))
      (voice 
       (cond ((zero? (mod i 3)) (detune greensleeves-chords 2))
             ((even? i) (add (detune pluck-bass 2)
                             (detune thunk-pluck 3)))
             (else pluck-bass)))
      (groove (* o (/ time (+ i a))))
      (do ((j 1 (+ o j)))
          ((> j (+ i a)))
        (format #t "~a~%" (list i j a))
        (play (quick-reduce (expt (+ s i) (+ a j)) a)))
      (rewind))))

(define-song "olvebran-guitar-tuning"
  (root 100)
  (tempo 60)
  (voice (adsr sin 0 1 0 0))
  (let ((play (index-edi play 2 15)))
    (groove 8)
    #; (0 3 5 6 9)
    (play 0 0 0 0)
    (play 9 9 9 9)
    (play 14 14 14 14)
    (play 20 20 20 20)
    (play )))

(define-song "olvebran-wood"
  (root 236)
  (tempo 65)
  (let* ((scale (vector-map
                 (snap-to-chroma (expt 2 1/15))
                 5-limit-major<3/2>))
         (play (pipe> play
                      (reduce-intervals 2)
                      (snap-to-scale scale))))
    (do ((s 0 (+ 1 s)))
        ((>= s 27))
      (format #t "s: ~a " s)
      (do ((m 0 (+ 1 m))
           (r (expt 3/2 s)
              (let ((new-value (expt 4/3 (round (quick-reduce (* r (+ 1 s m)) 16)))))
                (format #t "new-value: ~a~%" new-value)
                new-value)))
          ((>= m 3))
        (format #t "m: ~a " m)
        (do ((i 0 (+ 1 i)))
            ((>= i 3))
          (format #t "i: ~a~%" i)
          (flush-output-port (current-output-port))
          (let ((kk kick-drum2)
                (s1 snare-drum3)
                (s2 snare2)
                (hh high-hat))
            (groove 1/5)
            (rhythm
             play
             kk hh #f hh s1 hh #f hh s2 hh
             kk hh kk hh s1 hh s2 hh kk hh))
          (rewind 4)
          (voice pluck-bass)
          (groove 2/5 1/5 1/5 1/5)
          (let ((r (quick-reduce r 2)))
            (play r r #f (* r 3/4)
                  r r #f (* r 3/4)
                  r r #f (* r 3/4)
                  r r (* r 9/8) (* r (if (even? (+ i s m)) 3/2 3/4))))
          (rewind 4)
          (voice bell)
          (groove 3/5 1 1/5 1/5)
          (parameterize ((render-tail 3))
            (play #f
                  (map (lambda (n) (* r n))
                       '(3/2 15/8 9/4))
                  (if (zero? (mod (+ i s m) 5))
                      (* r 2)
                      #f)
                  (map (lambda (n) (* r n))
                       '(5/3 2/1))
                  #f
                  (map (lambda (n) (* r n))
                       '(3/2 5/3 9/4))
                  (if (even? (+ i s m))
                      (* r 2)
                      #f)
                  (map (lambda (n) (* r n))
                       '(15/8 5/2)))))
        (format #t "intermission: rhythm~%")
        (let ((kk kick-drum2)
              (s1 snare2)
              (s2 snare-drum3))
          (groove 1/5)
          (rhythm
           play
           kk #f kk #f s1
           #f #f #f s2 #f
           kk #f kk #f s1
           #f s2 s1 kk s2))
        (format #t "intermission: bass~%")
        (rewind 4)
        (voice pluck-bass)
        (groove 2/5 1/5 1/5 1/5)
        (play (* 9/8 r) (* 9/8 r) #f (* r 3/4)
              (* 9/8 r) (* 9/8 r) #f (* r 3/4)
              (* 9/8 r) (* 9/8 r) #f (* r 3/4)
              (* 9/8 r) (* 9/8 r) #f (* r 3/4))
        (rewind 4)
        (format #t "intermission: bell~%")
        (voice (detune bell 1/2))
        (groove 3/5 1 1/5 1/5)
        (format #t "chord 1~%")
        (play #f
              (map (lambda (n) (* r n))
                   '(3/2 9/4))
              (map (lambda (n) (* r n))
                   '(3/2 15/8 8/3))
              (map (lambda (n) (* r n))
                   '(3/2 7/3 8/3)))
        (format #t "chord 2~%")
        (play #f
              (map (lambda (n) (* r n))
                   '(3/2 9/4))
              (map (lambda (n) (* r n))
                   '(3/2 15/8 8/3))
              (map (lambda (n) (* r n))
                   '(3/2 7/3 8/3)))
        (format #t "chords over~%")))
    (voice (detune bell 1/2))
    (groove 8)
    (play '(1 3/2 2 5/2))
    (rewind 8)
    (voice pluck-bass)
    (groove 1)
    (play 1)
    (rewind 1)
    (voice kick-drum)
    (groove 1/8)
    (play 1)))

(define-song "polyrhythms"
  (root 330)
  (tempo 45)
  (let ((kk kick-drum)
        (sn snare-drum))
    (do ((i 0 (add1 i)))
        ((= i 32))
      (groove 1/8 1/8 1/4
              1/8 1/4 1/8)
      (rhythm play
              kk #f sn
              kk #f sn)))
  (rewind)
  (voice pluck-bass)
  (do ((i 0 (add1 i)))
      ((= i 4))
    (groove 3/8 1/8 1/8 3/8)
    (play 1 #f 7/8 9/10
          1 #f 7/8 9/10
          1 #f 7/8 9/10
          1 #f 7/8 9/10)
    (play 6/7 #f 3/4 27/35
          6/7 #f 3/4 27/35
          6/7 #f 3/4 27/35
          6/7 #f 3/4 27/35))
  (rewind)
  (render-tail 1)
  (voice (pipe> thick-fm-pad (detune 1/2)))
  (groove 4)
  (do ((i 0 (add1 i)))
      ((= i 2))
    (play '(3/2 9/4 27/8 81/32))
    (play '(4/3 16/9 64/27 128/81))
    (play '(5/4 5/3 2/1 8/3))
    (play '(9/8 27/16 81/40 243/80))))

;; the chords for bells.pcm16 in 19edo
;; (E G B D) (E A C E) (D A C F) (D G B F)
;; (A D F A) (E C G B) (G C E B) (C E F A)
;; (E A F C) (D A F B) (D F A B) (E G B B)
;; (E G C B) (A C E G) (E A C E) (G D F B)
;; (C D F A) (D D G B) (E C G B) (E C F A)
;; (E G G C) (F A C B) (D A F B) (E G B D)
;; (E A C F) (E A C F) (A C E G) (A D F A)
;; (A C F B) (D F G B) (D D G B) (E A F C)
(define-song "bells"
  (root 270)
  (let* ((max-iterations 64)
         (scale (rank-2-temperament 8 3/2 8))
         (play (snap-to-scale play scale)))
    (do ((i 0 (add1 i))
         (a '(1 5/4 3/2)
            (map (lambda (note)
                   (quick-reduce (* (add1 i) note) 2))
                 a))
         (b '(5/4 5/3 2/1)
            (map (lambda (note)
                   (quick-reduce (* (add1 i) note) 2))
                 c))
         (c '(9/8 4/3 5/3)
            (map (lambda (note)
                   (quick-reduce (* (add1 i) note) 2))
                 b))
         (d '(9/8 3/2 15/8)
            (map (lambda (note)
                   (quick-reduce (* (add1 i) note) 2))
                 d)))
        ((= i max-iterations))
      (format #t "starting iteration ~a/~a...~%"
              (add1 i)
              max-iterations)
      (groove 1/5)
      (render-tail 0)
      (format #t "	rhythm...~%")
      (let ((kk kick-drum)
            (sn snare-drum)
            (hh high-hat)
            (ld arp-lead)
            (bl bell)
            (bs pluck-bass))
        (rhythm play
                kk hh sn kk #f
                kk kk sn hh kk
                kk hh sn kk hh
                kk kk sn kk #f

                kk hh sn kk kk
                kk kk sn sn kk
                kk hh sn kk #f
                kk kk sn ld sn
                
                kk sn kk kk hh
                kk hh sn kk #f
                kk sn kk kk hh
                kk hh sn kk #f

                kk hh kk ld sn
                kk hh sn sn #f
                kk ld kk kk sn
                kk hh sn #f hh))
      (rewind 16)
      (voice bell)
      (groove 4)
      (render-tail 3)
      (format #t "	chords...~%")
      (cond ((zero? (mod i 7))
             (play (cons (* 2 (expt (caddr a) 2)) a)
                   (cons (* 2 (expt (caddr b) 2)) b)
                   (cons (* 2 (expt (caddr c) 2)) c)
                   (cons (* 2 (expt (caddr d) 2)) d)))
            ((zero? (mod i 5))
             (play (cons (* 2 (expt (cadr a) 3)) a)
                   (cons (* 2 (expt (cadr b) 3)) b)
                   (cons (* 2 (expt (cadr c) 3)) c)
                   (cons (* 2 (expt (cadr d) 3)) d)))
            ((zero? (mod i 3))
             (play (cons (* 3 (cadr a)) a)
                   (cons (* 3 (cadr b)) b)
                   (cons (* 3 (cadr c)) c)
                   (cons (* 3 (cadr d)) d)))
            (else
             (play a b c d)))
      (rewind 16)
      (voice pluck-bass)
      (render-tail 0)
      (format #t "	bass...~%")
      (let ((a (car a))
            (b (car b))
            (c (car c))
            (d (car d)))
        (groove 3/5 1/5 1/5
                1/5 3/5 1/5
                3/5 1/5 1/5
                1/5 2/5 2/5

                3/5 1/5 1/5
                1/5 3/5 1/5
                3/5 1/5 1/5
                1/5 4/5

                2/5 1/5 2/5
                3/5 1/5 1/5
                2/5 1/5 2/5
                3/5 1/5 1/5

                2/5 1/5 2/5
                4/5 1/5
                2/5 1/5 2/5
                3/5 1/5 1/5)
        (play a #f a
              (* 2 a) a (* 2 a)
              a #f (* 3/2 a)
              (* 2 a) a (quick-reduce (* 3/2 b) 2)

              b (* 2 b) b
              (* 2 b) b (* 2 b)
              b #f (* 3/2 b)
              b (quick-reduce (* 3/2 c) 2)

              c (* 2 c) c
              c #f (* 3/2 c)
              c (* 2 c) c
              c #f (quick-reduce (* 3/2 d) 2)

              d #f (* 3/2 d)
              d (* 2 d)
              d (* 2 d) (* 3/2 d)
              d #f (quick-reduce (* 3/2 a) 2))))))

(define-song "kilikili"
  (let ((p (pipe> play
                  (snap-to-scale 11-limit-thing)
                  (fjs-print))))
    (voice wobble-strings)
    (groove 2)
    (do ((i 0 (add1 i))
         (chords '((1 3/2 15/8 5/2)
                   (1 3/2 5/3 5/2)
                   (9/8 4/3 5/3 5/2)
                   (9/8 3/2 15/8 8/3)
                   (1 3/2 2 3)
                   (1 3/2 9/4 27/16)
                   (1 3/2 5/2 7/2)
                   (1 3/2 5/2 7/2))
                 (map (lambda (chord)
                        (map (lambda (note)
                               (quick-reduce (* 4/3 note) 4))
                             chord))
                      chords)))
        ((= i 8))
      (for-each p chords))))

(define-song "lemma"
  (let ((p (arpeggiate (snap-to-scale play 11-limit-thing) 3)))
    (root 200)
    (render-tail 3)
    (voice wobble-strings)
    (groove 1/4)
    (p '(1 5/4 3/2))
    (do ((i 0 (add1 i))
         (song `((9/8 5/3 2/1)
                 (9/8 5/3 2/1)
                 #f
                 (1 5/4 3/2)
                 (9/8 5/3 2/1)
                 (9/8 5/3 2/1)
                 #f
                 (1 5/4 3/2)
                 #f
                 4
                 #f
                 (4/3 5/3 9/4)
                 (5/4 5/3 9/4)
                 (4/3 5/3 9/4)
                 (5/4 5/3 9/4)
                 #f
                 (9/8 4/3 5/3)
                 (9/8 4/3 5/2)
                 (4/3 5/3 9/4)
                 (9/8 4/3 2/1))
               (map (lambda (chord)
                      (map (lambda (note)
                             (if (number? note)
                                 (quick-reduce (* note 11) 3)
                                 note))
                           (sort < (ensure-list chord))))
                    song)))
        ((= i 16))
      (for-each p song))))

(define-song "soft-math"
  (let* ((play (snap-to-scale play spicier-5-limit))
         (polyrhythm (lambda (dur div chord)
                       (groove (/ dur div))
                       (do ((i 0 (add1 i)))
                           ((>= i div))
                         (play chord))))
         (section (lambda (repeat notes)
                    (let ((dur 4))
                      (do ((j 0 (add1 j)))
                          ((>= j repeat))
                        (polyrhythm dur
                                    (ceiling (quick-reduce (fold-left + 0 notes) 16))
                                    notes))))))
    (root 120)
    (tempo 120)
    (render-tail 3)
    (voice triangle-pad)
    (do ((j 0 (add1 j))
         (offset 1 (if (zero? j)
                       (/ offset 3)
                       (* offset 3 j))))
        ((= j 16))
      (section 2 (map (lambda (n) (quick-reduce (* n offset) 5))
                      '(1 3 9 27)))
      (section 1 (map (lambda (n) (quick-reduce (* n offset) 6))
                      '(1 5 25 125)))
      (section 1 (map (lambda (n) (quick-reduce (* n offset) 5))
                      '(3 5 9 25)))
      (section 1 (map (lambda (n) (quick-reduce (* n offset) 6))
                      '(25 27 75 81)))
      (section 1 (map (lambda (n) (quick-reduce (* n offset) 4))
                      '(15 25 27 45))))))

(define-song "sinfwabe"
  (let ((play (snap-to-scale play '#(1 9/8 5/4 4/3 3/2))))
    (root 220)
    (render-tail 3)
    (voice saw-pad)
    (groove 2)
    (do ((i 0 (add1 i))
         (chord '(1 5/4 3/2)
                (map (lambda (n)
                       (quick-reduce
                        (expt 2 (sub1 n))
                        (if (even? i) 2 3)))
                     chord)))
        ((= i 32))
      (play chord))))

(define-song "sharp-noises"
  (let* ((play (snap-to-scale play '#(1 9/8 5/4 4/3 3/2))))
    (tempo 90)
    (root 200)
    (render-tail 3)
    (do ((i 1 (+ i 1)))
        ((= i 120))
      (groove (expt 3/2 (length (factors i))))
      (for-each (lambda (factor)
                  (voice thick-lead)
                  ((arpeggiate play (exact (floor (quick-reduce factor 12))))
                   (map (lambda (n) (quick-reduce (* factor (expt 3/2 (add1 n))) 3))
                        (iota factor)))
                  (rewind 1)
                  (voice thick-pad)
                  (play (list (quick-reduce factor 3)
                              (quick-reduce (* factor 81/64) 3)
                              (quick-reduce (* factor 3/2) 3)
                              (quick-reduce (* factor 16/9) 3))))
                (cdr (factors i))))))

(for-each (lambda (scale)
            (define-song
              (format "bedtime-pads-~a" (car scale))
              (let ((play (snap-to-scale play (cdr scale)))
                    (equave (vector-ref (cdr scale)
                                        (- (vector-length (cdr scale)) 1))))
                (voice thick-lead)
                (root 200)
                (render-tail 3)
                (do ((song '((1 5/4 3/2 15/8)
                             (2/3 5/6 1 5/4)
                             (5/6 1 5/4 3/2)
                             (3/4 15/16 9/8 4/3))
                           (map (lambda (chord)
                                  (map (lambda (note)
                                         (quick-reduce (* note note 5/4)
                                                       (expt equave 2)))
                                       chord))
                                song))
                     (i 0 (+ i 1)))
                    ((= i 8))
                  (pretty-print `(i ,i))
                  (unless (zero? i)
                    (groove (expt 3/2 i))
                    (pretty-print song)
                    (for-each (lambda (chord)
                                ((arpeggiate play (ceiling (* (length chord)
                                                              (expt 3/2 i))))
                                 (sort < chord)))
                              song))))))
          (list (cons 'over-11 (harmonic-series-segment 11 22))
                (cons 'over-19 (harmonic-series-segment 19 38))
                (cons '8-to-24 (harmonic-series-segment 8 24))
                (cons '5-limit-minor 5-limit-minor)
                (cons '5-limit-major 5-limit-major)))

(define-song "xtrobe10"
  (let ((play (retune-edi play 2 5)))
    (render-tail 3)
    (tempo 90)
    (root 89)
    (groove 1/8)
    (do ((i 0 (+ i 1))
         (a 2 b)
         (b 3 (quick-reduce (* 3 5 a b) 121))
         (chords '((1 5/4 3/2 15/8)
                   (1 5/4 3/2 7/4)
                   (1 5/4 3/2 5/3)
                   (1 5/4 5/3 9/4)
                   (1 4/3 5/3 5/2)
                   (9/8 4/3 5/3 5/2)
                   (9/8 5/3 2/1 5/2)
                   (9/8 3/2 15/8 8/3))
                 (map (lambda (chord)
                        (map (lambda (note)
                               (quick-reduce (* note a b) 5))
                             chord))
                      chords)))
        ((= i 512))
      (format #t "~a~%" (list (/ a b) b))
      (voice fm-pad)
      (apply (arpeggiate play 8) chords)
      (voice thick-lead)
      (apply (arpeggiate play 4)
             (reverse chords))
      (let ((m (quick-reduce (+ a b) 5)))
        (if (even? i)
            (modulate (retune-edi-note m 2 5))
            (modulate (retune-edi-note (/ m) 2 5)))))))

(define-song "strobe"
  (let ((melody
         '((1/1 12/5) (16/5)
           (1/1 8/3) (16/5)
           (2/3 2/1) (16/5)
           (2/3 3/1) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 9/4) (2/3 12/5)
           (18/5) (2/3 12/5)
           (18/5) (9/10 8/3)
           (18/5) (9/10 3/1)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (4) (1/1 9/4)
           (18/5) (1/1 12/5)
           (16/5) (1/1 9/4)
           (6/5 12/5) (4)
           (6/5 12/5) (24/5)
           (2/3 12/5) (16/5)
           (2/3 3/1) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 12/5) (16/5)
           (4/5 9/4) (12/5)
           (18/5) (2/3 12/5)
           (18/5) (2/3 8/3)
           (18/5) (9/10 3/1)
           (18/5) (9/10 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 12/5)
           (18/5) (1/1 9/4))))
    (tempo 90)
    (root 200)
    (render-tail 4)
    (voice thick-fm-pad)
    (groove 1/2 1/3)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (apply play melody))
    (let ((target (tape-head)))
      (voice bass-drum)
      (rewind)
      (groove (+ 1/2 1/3))
      (while (< (tape-head) target)
        (play 1)))
    (let ((target (tape-head)))
      (voice snare2)
      (rewind)
      (groove (+ 1/2 1/3))
      (while (< (tape-head) target)
        (play #f 1)))))

(define-song "boss"
  (root 230)
  (render-tail 2)
  (voice
   (pipe> sin
          (fm (add (pipe> sin (detune 2/4))
                   (pipe> sin (detune 3/4))
                   (pipe> sin (detune 5/4))
                   (pipe> sin (detune 7/4))))
          (add (pipe> sin
                      (detune 4)
                      (volume 1/4))
               (pipe> sin
                      (detune 3)
                      (volume 1/5))
               (pipe> sin
                      (detune 5)
                      (volume 1/6))
               (pipe> sin
                      (detune 7)
                      (volume 1/7)))
          (rm (pipe> sin
                     (detune 1/2)
                     (volume 1/2)
                     (offset 1/2)))
          (adsr 1/64 1 1/8 2)
          (add (pipe> triangle
                      (detune 1/4)
                      (volume 1)
                      (adsr 1/32 31/32 1/2 1/32)))))
  (groove 9)
  (play 2 5/2 9/4 16/9
        8/3 3 8/3 5/2)
  (rewind)
  (groove 72/14)
  (play #f #f 32/15 32/15 32/15 #f #f
        32/5 16/5 8/5 16/5 32/5 6 5)
  (rewind)
  (groove 4)
  (play 1 '(1 3/2)
        1 '(1 8/5)
        1 '(1 3/2)
        16/15 '(16/15 8/5)
        '(16/15 8/5) '(1 3/2)
        '(1 4/3) '(1 5/4)
        '(5/4 8/5) '(4/3 3/2)
        '(4/3 5/3) '(5/4 9/5)
        '(4/3 16/9) '(3/2 2)))

(define-song "pingy"
  (root 220)
  (render-tail 1)
  (voice pingy-lead)
  (groove 1/4)
  (do ((i 1 (+ i 1))
       (p play (snap-to-scale
                play (harmonic-series-segment i (* i 3))))
       (song '(1 3/2 27/16 #f 4/3 #f 4/3 #f
                 1 #f 3/2 27/16 3/2 27/16 4/3 #f
                 1 #f 3/2 27/16 3/2 27/16 4/3 #f
                 1 #f 27/16 #f 27/16 #f 27/16 #f 27/16 3/2)
             (map (lambda (note)
                    (if (number? note)
                        (quick-reduce (* 3/2 note) 3)
                        note))
                  song)))
      ((= i 16))
    (format #t "over ~a~%" i)
    (for-each p song)
    (tempo (* 32/31 (tempo)))))

(define-song "it-still-isnt"
  (let* ((scale mos-5L5s-9/4-equivalent-ultrasoft)
         (play (index-scale play scale)))
    (render-tail 2)
    (voice isnt-lead)
    (play 0 1 2 3 4 5 6 7 8 9)))

(define-song "water-isnt-wet"
  (let* ((scale mos-5L5s-9/4-equivalent-ultrasoft)
         (play (index-scale play scale)))
    (tempo 120)
    (root 220)
    (render-tail 1)
    (voice wet-lead)
    (groove 1 1 1
            1 2/3 2/3 2/3)
    (do ((j 0 (+ j 1)))
        ((= j 2))
      (do ((i 0 (+ i 1)))
          ((= i 2))
        (play 0 5 4
              0 5 4 2))
      (do ((i 0 (+ i 1)))
          ((= i 2))
        (play 1 6 5
              1 6 5 3)))))

(define-song "thunderkiss"
  (let* ((scale 5-limit-spicy)
         (play (index-scale play scale))
         (arp-sorting (make-cyclic-list (list < >)))
         (arp6 (arpeggiate play (* 32 32 6)))
         (arp3 (arpeggiate play (* 32 32 3))))
    (render-tail 0)
    (voice (pipe> sin
                  (adsr 0 0 1/2 0)))
    (groove 1 2 1)
    (do ((i 0 (+ i 1))
         (M 9 (* 3/2 M))
         (arps (make-cyclic-list (list arp3 arp6))
               (cdr arps))
         (chords '((0 2 4) (4 6 8))
                 (map (lambda (chord)
                        (set! arp-sorting (cdr arp-sorting))
                        (sort (car arp-sorting)
                              (map (lambda (note)
                                     (mod (+ i note)
                                          (ceiling (* 3/2 (vector-length scale)))))
                                   chord)))
                      chords)))
        ((= i 16))
      (for-each (car arps) chords))
    (play '(0 2 4 7))))

(define-song "arps"
  (let* ((play (index-scale play 5-limit-spicy))
         (arp (arpeggiate play 3))
         (modulate-by 9/7))
    (root 220)
    (render-tail 1)
    (voice fm-pad)
    (groove 2 1 1)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play 0 4 5
            #f 4 5
            6 5 4
            3 4 3
            #f 4 3
            2 5 4
            7 #f 6
            5 4 3)
      (modulate (expt modulate-by 4))
      (while (> (root) 440)
        (modulate 1/2)))
    (rewind)
    (render-tail 0)
    (root 220)
    (voice fm-triangle-bass)
    (groove 1/2 1/4 1/4
            1/2 1/2)
    (do ((i 0 (+ i 1)))
        ((= i 16))
      (play 0 4 4
            0 4
            0 4 4
            0 4
            0 4 4
            0 4
            0 #f #f
            #f 4)
      (modulate modulate-by)
      (while (> (root) 440)
        (modulate 1/2)))
    (rewind)
    (root 220)
    (render-tail 3)
    (voice (pipe> square
                  (volume 1/16)
                  (adsr 0 0 1 3)))
    (groove 1/4 1/8 1/8)
    (do ((j 0 (+ j 1)))
        ((= j 16))
      (do ((i 0 (+ i 1)))
          ((= i 4))
        (arp #f '(0 2 5) #f
             #f '(0 2 5) #f
             #f '(0 3 5) #f
             #f '(0 3 5) #f))
      (modulate modulate-by)
      (while (> (root) 440)
        (modulate 1/2)))))

(define-song "something-else"
  (let ((play (index-scale play '#(1 8/7 9/7 4/3 3/2 27/16 27/14 2/1))))
    (root 120)
    (groove 1/4)
    (render-tail 8)
    (voice shiny-sin)
    (do ((i 0 (+ i 1))
         (notes (list 0 2 4 7 9 7 4 2
                      1 3 5 8 9 8 5 3
                      1 4 7 9 11 9 7 4)
                (map (lambda (n)
                       (mod (+ 1 (* n 31)) 12))
                     notes)))
        ((= i 4)
         (play '[0 2 4 8]))
      (apply play notes))))

(define-song "something-pretty"
  (let ((play (index-edi play 2/1 17)))
    (root 120)
    (groove 1/4)
    (render-tail 8)
    (voice pretty-pad)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play '[9 19] 13 19 20
            23 26 23 20
            19 13  9  6
            9 13 19 20
            23 26 23 20
            19 13  9  6

            3  6 10 16
            20 23 26 23
            20 16 10  6
            '[3 13] 6 10 16
            20 23 26 23
            20 16 10  6

            '[9 29] 13 '[19 26] 20
            23 26 23 20
            '[19 26] 13 9 6
            '[9 29] 13 19 20
            23 26 23 20
            '[19 23] 13  9  6

            '[3 23] 6 10 16
            20 23 26 23
            '[20 26] 16 '[10 19]  6
            '[3 13] 6 10 16
            20 23 26 23
            20 16 '[10 16] 6))))

(define-song "local-thunk"
  (let ((play (index-scale play 7-limit-thing)))
    (tempo 130)
    (root 180)
    (render-tail 1)
    (voice thunk-pluck)
    (groove 1/2 1/2 1 1)
    (do ((i 0 (+ i 1))
         (chord (list 0 7 11)
                (map (lambda (n) (mod (+ n 3) 11))
                     chord)))
        ((= i 31))
      (play chord)
      (play (mod (+ 4 (fold-left max 0 chord)) 12)
            (mod (+ 17 (fold-left min 0 chord)) 12)))
    (play 0 0)))

(define-song "more-beeses"
  (root 100)
  (render-tail 1)
  (voice old-bee-pad)
  (groove 4)
  (for-each (lambda (edi)
              ((apply retune-edi play edi)
               '(1 3/2 15/8 5/2)
               '(1 4/3 2/1 5/2)
               '(1 3/2 15/8 5/2)
               '(9/8 5/3 2/1 5/2)
               '(5/4 5/3 2/1 3/1)
               '(4/3 5/3 2/1 8/3)
               '(4/3 5/3 2/1 5/2)
               '(9/8 3/2 15/8 9/4))
              ((apply retune-edi play edi)
               '(1 3/2 15/8 5/2 3/1)
               '(1 4/3 2/1 5/2 8/3)
               '(1 3/2 15/8 5/2 3/1)
               '(9/8 5/3 2/1 5/2 10/3)
               '(5/4 5/3 2/1 3/1 3/1)
               '(4/3 5/3 2/1 8/3 4/1)
               '(4/3 5/3 2/1 5/2 15/4)
               '(9/8 3/2 15/8 9/4 9/2))
              ((apply retune-edi play edi)
               '(1 3/2 15/8 5/2 5/1)
               '(1 4/3 2/1 5/2 16/3)
               '(1 3/2 15/8 5/2 5/1)
               '(9/8 5/3 2/1 5/2 9/2)
               '(5/4 5/3 2/1 3/1 5/1)
               '(4/3 5/3 2/1 8/3 20/3)
               '(4/3 5/3 2/1 5/2 20/3)
               '(9/8 3/2 15/8 9/4 15/2))
              ((apply retune-edi play edi)
               '(1 3/2 15/8 5/2 8/1)
               '(1 4/3 2/1 5/2 15/2)
               '(1 3/2 15/8 5/2 20/3)
               '(9/8 5/3 2/1 5/2 6/1)
               '(5/4 5/3 2/1 3/1 5/1)
               '(4/3 5/3 2/1 8/3 9/2)
               '(4/3 5/3 2/1 5/2 4/1)
               '(9/8 3/2 15/8 9/4 15/4))
              ((apply retune-edi play edi)
               '(1 3/2 2/1 5/2))
              (rewind))
            '((2 12)
              (3/2 10))))

(define-song "this-genre"
  (root 320)
  (render-tail 1)
  (let ((play (pipe> play (retune-edi 3/2 4))))
    ;; lead
    (voice square-lead)
    (groove 1/2 1/4 1/4)
    ((fjs-print play)
     #f #f #f
     1 9/8 5/4
     #f 4/3 5/4
     3/2 #f #f
     3/2 4/3 3/2
     #f #f 3/2
     5/3 3/2 #f
     3/2 4/3 5/4
     4/3 5/4 9/8
     #f #f #f
     4/3 5/4 9/8
     #f #f 4/3
     5/4 #f 9/8
     #f 5/4 #f
     4/3 3/2 #f
     3/2 5/3 #f
     #f 2 #f
     1 9/8 5/4
     #f 4/3 5/3
     3/2 #f #f
     5/3 3/2 4/3
     #f #f 5/4
     4/3 5/4 #f
     9/8 #f 5/4
     #f 5/3 #f
     #f 5/3 3/2
     #f 5/3 3/2
     5/4 4/3 3/2
     #f 5/3 #f
     3/2 #f 4/3
     #f 5/4 #f
     9/8 5/4 #f
     1 #f #f)
    (rewind)
    (do ((loop 0 (+ loop 1)))
        ((= loop 2))
      ;; kick drum
      (voice kick-drum2)
      (groove 1/4)
      (do ((i 0 (+ i 1)))
          ((= i 4))
        (play 1 #f 1 #f
              1 #f 1 #f
              1 #f 1 #f
              1 #f 1 #f))
      (rewind 16)
      ;; ducked bass
      (voice ducked-bass)
      (play #f 1 #f 1
            #f 1 #f 1
            #f 1 #f 1
            #f 1 #f 1)
      (play #f 3/4 #f 3/4
            #f 3/4 #f 3/4
            #f 3/4 #f 3/4
            #f 3/4 #f 3/4)
      (play #f 5/6 #f 5/6
            #f 5/6 #f 5/6
            #f 5/6 #f 5/6
            #f 5/6 #f 5/6)
      (play #f 15/16 #f 15/16
            #f 15/16 #f 15/16
            #f 15/16 #f 15/16
            #f 15/16 #f 15/16)
      (rewind 16)
      ;; soft chords
      (voice soft-chords)
      (groove 3/2 5/2)
      ((fjs-print play)
       '(1 3/2 15/8 5/2 4)
       '(1 3/2 15/8 9/4 15/4)
       '(9/8 3/2 15/8 8/3)
       '(9/8 5/3 2/1 8/3)
       '(9/8 5/3 2/1 45/16 9/2)
       '(5/4 5/3 2/1 3/1 4)
       '(5/4 15/8 9/4 3/1)
       '(9/8 4/3 15/8 10/3)))))

(define-song "whirling"
  (root 200)
  (tempo 440)
  (render-tail 2)
  (voice whirling-in-rags-pad)
  (groove 1 1 12
          1 1 2 2 8
          1 1 12
          1 1 2 2 8
          1 1 12
          1 1 2 2 8
          14)
  (play
   1 9/8 5/4
   1 9/8 5/4 4/3 9/8
   1 9/8 5/4
   1 9/8 5/4 3/4 5/6
   1 9/8 5/4
   1 9/8 5/4 4/3 9/8
   1)
  (rewind)
  (modulate 2/3)
  (play
   3/2 5/3 15/8
   3/2 5/3 15/8 2/1 5/3
   3/2 5/3 15/8
   3/2 5/3 15/8 9/8 5/4
   3/2 5/3 15/8
   3/2 5/3 15/8 2/1 5/3
   3/2))

(define-song "returned-to-mrzrek"
  (let ((play (pipe> play
                     (retune-edi 3/2 4)
                     (fjs-print))))
    (render-tail 4)
    (root 169)
    (voice mrzek-pluck)
    (groove 1/2)
    (let ((chords '((1 3/2 15/8 5/2)
                    (1 3/2 9/5 5/2)
                    (1 3/2 7/4 5/2)
                    (1 4/3 5/3 5/2)
                    (15/16 45/32 105/64 75/32)
                    (5/4 15/8 9/4 3/1)
                    (5/4 5/3 2/1 3/1)
                    (9/8 5/3 2/1 8/3)
                    (9/8 3/2 15/8 8/3)
                    (1 3/2 7/4 5/2))))
      (do ((i 0 (+ i 1)))
          ((= i 8))
        (apply play chords)
        (set! chords (map (lambda (chord)
                            (map (lambda (note)
                                   (inexact (interval-reduce (* note 4/3) 4)))
                                 chord))
                          chords))))))

(define-song "absolutely"
  (let ((song (lambda (play mutate-note)
                (let ((notes '(1 3/2 15/8 5/2)))
                  (root 210)
                  (tempo 80)
                  (groove 1/2 1/2 1/2 1/2
                          1/2 1/2 1/2 3/2)
                  (render-tail 4)
                  (voice absolutely-lead)
                  (do ((i 0 (+ i 1)))
                      ((= i 8))
                    (play notes)
                    (set! notes (map mutate-note notes)))))))
    (let* ((play4 (pipe> play
                         (reduce-intervals 4/1)
                         (retune-edi 3/2 10)
                         (fjs-print)))
           (play3 (pipe> play
                         (reduce-intervals 3/1)
                         (retune-edi 3/2 10)
                         (fjs-print)))
           (play5 (pipe> play
                         (reduce-intervals 5/2)
                         (retune-edi 3/2 10)
                         (fjs-print)))
           (incr (lambda (n)
                   (lambda (x)
                     (inexact (* x n)))))
           (block (lambda (n)
                    (song play4 (incr n))
                    (song play3 (incr n))
                    (song play4 (incr n))
                    (song play5 (incr n)))))
      (block 3/4)
      (block 20)
      (block 21))))

(define-song "beeses"
  (let ((play (retune-edi play 3/2 10)))
    (root 100)
    (render-tail 1)
    (voice old-bee-pad)
    (groove 4)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play '(1 3/2 15/8 5/2)
            '(1 4/3 2/1 5/2)
            '(1 3/2 15/8 5/2)
            '(9/8 5/3 2/1 5/2)
            '(5/4 5/3 2/1 3/1)
            '(4/3 5/3 2/1 8/3)
            '(4/3 5/3 2/1 5/2)
            '(9/8 3/2 15/8 9/4)))))

(define-song "you-chose-to-be"
  (let ((play (pipe> play
                     (reduce-intervals 4)
                     (retune-edi 3/2 5)
                     (fjs-print)))
        (n 3/4))
    (root 420)
    (tempo 120)
    (render-tail 2)
    (voice (add (pipe> sin
                       (adsr 1/32 1/4 1/128 2)
                       (volume 1/2))
                (pipe> sin
                       (adsr 0 1/2 1/128 2)
                       (detune 2))
                (pipe> sin
                       (adsr 1/32 1/8 1/64 1)
                       (volume 2/3)
                       (detune 1/2))))
    (groove 1/2)
    (let ((chords '(10 15 18 25)))
      (do ((i 0 (+ i 1)))
          ((= i 32))
        (do ((j 0 (+ j 1)))
            ((>= j (floor (interval-reduce n 4))))
          (for-each play chords))
        (set! chords (map (lambda (chord)
                            (map (lambda (note)
                                   (interval-reduce (inexact (* note n)) 256))
                                 (ensure-list chord)))
                          chords))
        (apply groove (cons (/ (+ 1 (mod i 4)) 4) (groove)))))))

(let ((play (fjs-print play)))
  (define-song "the-circle-closes"
    (root 200)
    (tempo 130)
    (groove 1 1/2)
    (render-tail 1)
    (voice (pipe> triangle
                  (adsr 1/32 2/3 1/8 1)))
    (do ((n 10 (- n 1)))
        ((<= n 3))
      (let ((play (retune-edi play 3/2 n)))
        (play '(1 3/2 15/8 5/2) 5/2
              '(1 3/2  9/5 5/2) 9/4
              '(1 3/2  5/3 9/4) 2/1
              '(5/6 5/4 3/2 2/1) 9/4
              '(5/6 9/8 4/3 2/1) 15/8
              '(3/4 9/8 4/3 15/8) 15/8
              '(3/4 1/1 5/4 15/8) 7/4
              '(3/4 1/1 5/4 7/4) 9/4)
        (modulate 2/3)
        (when (< (root) 100)
          (modulate 2))))))

(define-song "crunchy-nuts"
  (let ((play (retune-edi play 2 11)))
    (root 200)
    (render-tail 1)
    (voice (pipe> sin
                  (adsr 1/32 2/3 1/4 1/3)))
    (groove 1 1/2)
    (do ((i 0 (+ i 1)))
        ((= i 2))
      (play '(1/1 3/2 15/8 5/2) 9/4
            '(1/1 3/2 15/8 5/2) 9/4
            '(15/8 4/3 5/3 9/4) 9/4
            '(5/4 15/8 9/4 3/1) 5/2
            '(5/4 5/3 2/1 9/4)  8/3
            '(9/8 5/3 2/1 8/3)  8/3
            '(9/8 3/2 15/8 8/3) 5/2
            '(5/6 5/4 3/2 2/1)  9/4))
    (play '(1/1 3/2 15/8 5/2) 5/2
          '(1/1 3/2 15/8 5/2) 8/3
          '(15/8 4/3 5/3 9/4) 3/1
          '(5/4 15/8 9/4 3/1) 8/3
          '(5/4 5/3 2/1 9/4)  8/3
          '(9/8 5/3 2/1 8/3)  3/1
          '(9/8 3/2 15/8 8/3) 3/1
          '(9/8 3/2 15/8 8/3) 5/2)
    (groove 2)
    (play '(1 3/2 2 5/2))))


(define-song "8edf-morning"
  (let ((play (retune-edi play 3/2 8)))
    (root 20)
    (render-tail 1)
    (voice morning-lead)
    (groove 8/5)
    (play 16 15 13 12 #f
          16 15 13 12 #f
          16 15 16 18 #f
          16 18 16 15 13
          12 #f 13 #f #f
          12 #f 13 15 #f
          12 #f 13 16 #f
          12 #f 13 15 13
          10)
    (rewind)
    (render-tail 0)
    (voice (pipe> sin
                  (volume 1/3)
                  (adsr 0 2/3 0 0)))
    (groove 4)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play '(10 13 15)
            '(8 12 16)
            '(10 13 15)
            '(6 12 18)))
    (rewind)
    (voice morning-bass)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play 10 8 10 6))))

(define-song "10edf-morning"
  (let ((play (retune-edi play 3/2 10)))
    (root 20)
    (render-tail 1)
    (voice morning-lead)
    (groove 8/5)
    (play 16 15 13 12 #f
          16 15 13 12 #f
          16 15 16 18 #f
          16 18 16 15 13
          12 #f 13 #f #f
          12 #f 13 15 #f
          12 #f 13 16 #f
          12 #f 13 15 13
          10)
    (rewind)
    (render-tail 0)
    (voice (pipe> sin
                  (volume 1/3)
                  (adsr 0 2/3 0 0)))
    (groove 4)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play '(10 13 15)
            '(8 12 16)
            '(10 13 15)
            '(6 12 18)))
    (rewind)
    (voice morning-bass)
    (do ((i 0 (+ i 1)))
        ((= i 4))
      (play 10 8 10 6))))

(define-song "sleepy121-81"
  (root 220)
  (tempo 60)
  (render-tail 2)
  (voice sleepy-saw-pad)
  (let ((play (index-edi play 11/9 1)))
    (groove 4)
    (play -2)
    (play '(0 1 3)) (modulate 9/11) (play -2)
    (play '(0 1 3)) (modulate 9/11) (play 2)
    (play '(0 1 3)) (modulate 9/11) (play -2)
    (play '(0 1 3)) (modulate 11/9) (play 2)
    (play '(0 1 3)) (modulate 11/9) (play -2)
    (play '(0 1 3)) (modulate 11/9) (play 2)

    (play '(0 1 3)) (modulate 9/11) (play -2)
    (play '(0 1 3)) (modulate 9/11) (play 2)
    (play '(0 1 3)) (modulate 9/11) (play -2)
    (play '(0 1 3)) (modulate 11/9) (play 2)
    (play 1 0 #f #f #f))

  (rewind)

  (root 220)
  (render-tail 3)
  (voice sleepy-sin-pad)
  (groove 1)
  (let ((play (index-edi play 121/81 3)))
    (do ((i 0 (+ 1 i)))
        ((= i 15))
      (play '(0 3 6) 9
            '(-3 0 6) 7))
    (play #f #f #f #f))

  (rewind)

  (root 220)
  (render-tail 1)
  (voice (pipe> square
                (volume 1/90)
                (adsr 3/4 1/32 2/11 1)
                (detune 4)))
  (groove 4)
  (play #f)
  (groove 1/32)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   5 #f #f #f
   4 #f #f #f
   3 #f #f #f
   2 #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)

  (modulate 8/9)

  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   3 4 3 4
   5 6 5 6
   3 4 3 4
   5 4 5 4
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   5 4 3 2
   1 2 3 4
   5 4 3 2
   1 2 3 4
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   10 #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   9 #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   8 #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5

   #f #f #f #f
   #f #f #f #f
   5 10 #f #f
   20 #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)

  (modulate 8/9)

  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (play
   1 2 1 2
   2 3 2 3
   3 4 3 4
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)

  (modulate 8/9)

  (play
   1 3 4 3
   1 4 5 4
   1 2 3 2
   1 3 4 3
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/4)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 3/2)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 8/9)
  (modulate 4/3)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 5 4 5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 15/16)
  (play
   1 3 2 3
   2 1 2 1
   3 2 3 2
   4 24/5 4 24/5
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f)
  (modulate 9/10)
  (play
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   #f #f #f #f
   #f #f #f #f
   #f #f #f #f
   #f #f #f #f

   9 #f #f #f
   8 #f #f #f
   #f #f #f #f
   #f #f #f #f))

(define-song "mush"
  (root 130)
  (voice mush-pad)
  (render-tail 1)
  (groove 2/3 1/3)
  (play '(1 5/4 3/2) 2)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (do ((j 0 (+ 1 j)))
        ((= j 2))
      (play '(5/3 2 5/4) 15/8
            '(5/6 9/4 4/3) 5/3
            '(3/2 15/8 9/8) 3/2
            '(1 5/4 3/2) 5/3))
    (do ((j 0 (+ 1 j)))
        ((= j 2))
      (play '(5/3 2 5/2) 2
            '(5/3 9/8 8/3) 5/2
            '(3/2 15/8 9/4) 8/3
            '(1 5/2 3/2) 3))
    (play '(5/4 5/3 3) 10/3
          '(4/3 2 10/3) 3
          '(3/2 9/4 8/3) 5/2
          '(3/2 15/8 5/2) 9/4
          '(4/3 2 9/4) 5/2
          '(5/4 5/3 5/2) 8/3
          '(9/4 5/3 4/3) 9/4
          '(1 3/2 5/2) 15/8)))

(define-song "four-fifths-of-eleven"
  (root 199/11)
  (tempo 45)
  (voice (pipe> sin
	        (detune 1/256)
	        (volume 1/3)
	        (adsr 0 1 0 0)))
  (groove 32)
  (play '(11 12 13))
  (rewind)
  (voice eleven-pad)
  (groove 4)
  (play '(17 21 31)
        '(18 23 32)
        '(17 21 33)
        '(15 20 34)
        '(16 24 35)
        '(17 23 34)
        '(18 22 33)
        '(17 21 32))
  (rewind)
  (voice eleven-pluck)
  (groove 1/4)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (play 11 15 18 16 17 #f 10 #f)
    (play 11 15 18 16 17 #f 10 #f)
    (play 11 15 18 16 17 #f 10 #f)
    (play 11 22 21 20 19 #f 18 #f))
  (rewind)
  (groove 2 1 1)
  (voice eleven-bass)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (play 11 11 11
	  11 13 12)))

(define-song "forz"
  (root 199/23)
  (tempo 60)
  (voice (pipe> sin (adsr 1/32 1/8 1/4 4)))
  (render-tail 4)
  (groove 1/4)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (play 23 33 44 55 66 65
	  26 36 46 56 67 66
	  29 39 49 59 70 69
	  27 37 47 57 68 67))
  (rewind)
  (voice forz-bass)
  (groove 3/4)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (play '(23 34) '(47 70)
	  26 53
	  29 69
	  27 55)))

(define-song "giant-stepping-9edo"
  (let ((play (retune-edi play 2 9))
        (chord:Eb   '(1 5/4 3/2 2))
        (chord:B    '(4/5 1 6/5 8/5))
        (chord:G    '(5/4 25/16 15/8 5/2))
        (chord:Bb7  '(3/2 15/8 9/4 21/8))
        (chord:Gb7  '(6/5 3/2 9/5 21/10))
        (chord:D7   '(15/16 75/64 45/32 105/64))
        (chord:Am7  '(7/5 42/25 21/10 63/25))
        (chord:Fm7  '(9/8 27/20 27/16 81/40))
        (chord:Dbm7 '(7/8 35/32 21/16 49/32))
        (note:A 7/5)
        (note:B 8/5)
        (note:Bb 3/2)
        (note:Bb2 3/1)
        (note:D 15/8)
        (note:Eb 1)
        (note:Eb2 2)
        (note:F 9/8)
        (note:G 5/4)
        (note:G2 5/2)
        (note:Gb 6/5)
        (note:Gb2 12/5))
    (root 230)
    (tempo 170)
    ;; melody
    (voice greensleeves-lead)
    (groove 1 1 1 2/3 4/3 1 2/3 4/3)
    (play note:Gb2 note:D
	  note:B note:G note:Bb #f
	  note:B note:A)
    (groove 1 1 1 2/3 4/3 1 1 2/3 4/3 1)
    (play note:D note:Bb
	  note:G note:Eb note:Gb #f
	  note:G note:F note:Bb #f)
    (groove 1 2/3 4/3 1
	    1 2/3 7/3
	    1 2/3 7/3
	    2/3 2/3)
    (play note:B note:A note:D #f
	  note:Eb2 note:Eb2 note:Gb2
	  note:G2 note:G2 note:Bb2
	  note:Gb2 note:Gb2)
    ;; chords
    (rewind)
    (voice greensleeves-chords)
    (groove 1 1 1 2/3 7/3 2/3 4/3)
    (play chord:B chord:D7
	  chord:G chord:Bb7
	  chord:Eb
	  chord:Am7 chord:D7)
    (play chord:G chord:Bb7
	  chord:Eb chord:Gb7
	  chord:B
	  chord:Fm7 chord:Bb7)
    (groove 2 2/3 4/3)
    (play chord:Eb chord:Am7 chord:D7
	  chord:G chord:Dbm7 chord:Gb7
	  chord:B chord:Fm7 chord:Bb7
	  chord:Eb chord:Dbm7 chord:Gb7)
    (values)))

(define-song "cadences"
  (let ((play (retune-edi play 3/2 8)))
    (root 120)
    (tempo 120)
    (voice (pipe> sin (adsr 0 1 0 0)))
    (do ((j 0 (+ 1 j)))
        ((= j 8))
      (do ((i 0 (+ 1 i)))
	  ((= i 2))
        (groove 1 1/2)
        (play '(1 3/2 15/8 5/2) 2
	      '(1 3/2 5/3 5/2) 2
	      '(1 3/2 15/8 5/2) 2
	      '(1 3/2 5/3 5/2) #f
	      '(5/6 5/4 3/2 2/1) 5/2
	      '(5/6 5/4 3/2 2/1) 5/2
	      '(5/6 9/8 4/3 2/1) 9/4
	      '(5/6 9/8 4/3 2/1) #f)
        (modulate 2/3)
        (when (< (root) 90)
	  (modulate 2)))
      (groove 3/2)
      (play '(5/4 5/3 2/1 3/1)
	    '(9/8 5/3 2/1 8/3)
	    '(9/8 3/2 15/8 8/3)
	    '(9/8 3/2 15/8 8/3))
      (play '(5/4 5/3 2/1 3/1)
	    '(9/8 5/3 2/1 8/3)
	    '(9/8 3/2 15/8 8/3)
	    '(9/8 3/2 15/8 8/3)))))

(define-song "draguler"
  (root 120)
  (tempo 60)
  (voice draguler)
  (groove 1/2)
  (play 1 1 1
        1 1 7/6
        1 1 1
        1 1 1
        1 1 4/5 9/10))

(define-song "frogs-in-fours"
  (let ((play (pipe> play (index-edi 3/2 4))))
    (root 120)
    (tempo 110)
    ;; lead
    (voice frog-lead)
    (groove 4 1/2 1/2 1/2 1/2
	    3/2 3/2 3)
    (play 12 #f 12 15 #f
	  16 15 16)
    (play 12 #f 12 15 #f
	  16 15 17)
    (play 12 #f 12 15 #f
	  16 15 16)
    (play 12 #f 12 15 #f
	  16 17 16)
    ;; kick
    (rewind)
    (voice kick-drum3)
    (groove 3/4)
    (play '(1 1) #f 1 #f
	  1 #f 1 #f
	  '(1 1) #f 1 #f
	  1 #f 1 #f)
    (play '(1 1) #f 1 #f
	  1 #f 1 #f
	  '(1 1) #f 1 #f
	  1 #f 1 #f)
    (play '(1 1) #f 1 #f
	  1 #f 1 #f
	  '(1 1) #f 1 #f
	  1 #f 1 #f)
    (play '(1 1) #f 1 #f
	  1 #f 1 #f
	  '(1 1) #f 1 #f
	  1 #f 1 #f)
    ;; snare
    (rewind)
    (voice snare-drum3)
    (groove 3/4)
    (play #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1)
    (play #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1)
    (play #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1)
    (play #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1)
    ;; bass
    (rewind)
    (voice frog-bass)
    (groove 5/4 1 3/4)
    (play 0 2 4  0 2 4
	  0 3 5  0 3 5)
    (play 0 2 4  0 2 4
	  0 3 5  0 3 5)
    (play 0 2 4  0 2 4
	  0 3 5  0 3 5)
    (play 0 2 4  0 2 4
	  0 3 5  0 3 5)
    ;; chord stabs
    (rewind)
    (voice frog-stabs)
    (do ((i 0 (+ 1 i)))
        ((= i 4))
      (groove 3/2 3/4 3/4)
      (play '(4 8) '(4 8 12) '(4 8))
      (groove 3/2 1/2 1)
      (play '(4 8) '(4 8 12) '(4 8))
      (groove 3/2 3/4 3/4)
      (play '(5 8) '(5 8 10) '(5 8))
      (groove 3/2 1/2 1)
      (play '(5 8) '(5 8 10) '(5 8)))))

(define-song "by-sixes"
  (root 200)
  (tempo 60)
  (voice (pipe> sin (adsr 0 1 0 0)))
  (groove 1 1 1 3/2
	  1 3/2 1 1
	  1 3/2 1 3/2
	  1 1 1 1)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (play '(5/5 5/4 5/3 5/2)
	  '(5/5 5/4 5/3 5/2)
	  '(5/5 5/3 5/2 3/1)
	  '(5/5 5/4 5/3 8/3)
	  '(5/6 5/5 5/4 5/3)
	  '(5/6 5/5 5/4 5/3)
	  '(5/6 5/4 5/3 15/5)
	  '(5/6 5/5 5/3 5/2))
    (modulate 5/6)
    (when (< (root) 110)
      (modulate 2))
    (play '(3/4 3/2 15/8 9/4)
	  '(3/4 9/8 4/3 15/8))))

(define-song "hotel-california-Am-2d14"
  (let ((play (retune-edi play 2 14)))
    (root 190)
    (tempo 60)
    (voice california-pad)
    (groove 2 2 1 1 1 1)
    (play 1 1 4/5 4/5 9/10 9/10)))

(define-song "greensleeves-Dm-4edf"
  (let ((play (retune-edi play 3/2 4)))
    (root 220)
    (tempo 60)
    ;; melody
    (voice (pipe> sin (adsr 1/32 15/16 0 0)))
    (modulate 2)
    (groove 1/4)
    (play #f #f 1)
    (groove 1/2 1/4 3/8 1/8 1/4)
    (play 6/5 4/3 3/2 8/5 3/2
	  4/3 10/9 9/10 1 10/9
	  6/5 1 1 15/16 1)
    (groove 1/2 1/4)
    (play 10/9 15/16
	  3/4 1)
    (groove 1/2 1/4 3/8 1/8 1/4)
    (play 6/5 4/3 3/2 8/5 3/2
	  4/3 10/9 9/10 1 10/9)
    (groove 3/8 1/8 1/4
	    1/4 1/4 1/4)
    (play 6/5 10/9 1
	  15/16 4/5 15/16)
    (groove 1/2 1/4)
    (play 1 1)
    (groove 3/4)
    (play 1 9/10)
    (groove 1/4)
    (play 9/5 8/5 3/2)
    (groove 1/2 1/4 3/8 1/8 1/4)
    (play 4/3 10/9 9/10 1 10/9)
    ;; chords
    (rewind)
    (modulate 1/2)
    (groove 3/4)
    (play #f)
    (play '(1 6/5 3/2)
	  '(1 6/5 3/2)
	  '(9/10 10/9 4/3)
	  '(9/10 10/9 4/3)
	  '(1 6/5 3/2)
	  '(1 6/5 3/2)
	  '(3/4 15/16 9/8 4/3)
	  #f
	  '(1 6/5 3/2)
	  '(1 6/5 3/2)
	  '(9/10 10/9 4/3)
	  '(9/10 10/9 4/3)
	  '(1 6/5 3/2)
	  '(1 6/5 3/2)
	  '(3/4 15/16 9/8 4/3)
	  '(1 6/5 3/2)
	  '(1 6/5 3/2))
    (play '(6/5 3/2 9/5)
	  '(6/5 3/2 9/5)
	  '(9/10 10/9 4/3)
	  '(9/10 10/9 4/3))))

(define-song "greensleeves-C-2d12"
  (let ((play (retune-edi play 2 12)))
    (root (* 220 8/9))
    (tempo 60)
    ;; melody
    (voice (pipe> sin (adsr 1/32 15/16 0 0)))
    (modulate 2)
    (groove 1/4)
    (play #f #f 9/8)
    (groove 1/2 1/4 3/8 1/8 1/4)
    (play 4/3 3/2 5/3 15/8 5/3
	  3/2 5/4 1 9/8 5/4
	  4/3 9/8 9/8 16/15 9/8)
    (groove 1/2 1/4)
    (play 5/4 16/15
	  5/6 9/8)
    (groove 1/2 1/4 3/8 1/8 1/4)
    (play 4/3 3/2 5/3 15/8 5/3
	  3/2 5/4 1 9/8 5/4)
    (groove 3/8 1/8 1/4
	    1/4 1/4 1/4)
    (play 4/3 5/4 9/8
	  16/15 15/16 16/15)
    (groove 1/2 1/4)
    (play 9/8 9/8)
    (groove 3/4)
    (play 9/8 1)
    (groove 1/4)
    (play 2 15/8 5/3)
    (groove 1/2 1/4 3/8 1/8 1/4)
    (play 3/2 5/4 1 9/8 5/4)
    ;; chords
    (rewind)
    (modulate 1/2)
    (groove 3/4)
    (play #f)
    (play '(9/8 4/3 5/3)
	  '(9/8 4/3 5/3)
	  '(1 5/4 3/2)
	  '(1 5/4 3/2)
	  '(9/8 4/3 5/3)
	  '(9/8 4/3 5/3)
	  '(5/6 16/15 5/4 3/2)
	  #f
	  '(9/8 4/3 5/3)
	  '(9/8 4/3 5/3)
	  '(1 5/4 3/2)
	  '(1 5/4 3/2)
	  '(9/8 4/3 5/3)
	  '(9/8 4/3 5/3)
	  '(5/6 16/15 5/4 3/2)
	  '(9/8 4/3 5/3)
	  '(9/8 4/3 5/3))
    (play '(4/3 5/3 2/1)
	  '(4/3 5/3 2/1)
	  '(1 5/4 3/2)
	  '(1 5/4 3/2))))

(define-song "ringing"
  (let ((play (retune-edi play 3 13)))
    (root 120)
    (tempo 90)
    (voice (pipe> sin (adsr 1/32 15/16 0 0)))
    (groove 3 1)
    (play '(1 9/7 32/21 96/49) 28/9
	  '(7/9 1 32/27 32/21) 18/7)
    (groove 3 1/2 1/2)
    (play '(1 9/7 32/21 96/49) 35/9 28/9
	  '(7/9 1 32/27 32/21) 27/7 18/7)
    (groove 2 1 1)
    (play '(1 9/7 32/21 96/49) 35/9 28/9
	  '(7/9 1 32/27 32/21) 27/7 18/7)
    (groove 1)
    (play '(1 9/7 32/21 96/49) 42/9 35/9 28/9
	  '(7/9 1 32/27 32/21) 36/7 27/7 18/7)))

(define-song "scales"
  (root 120)
  (tempo 60)
  (voice (pipe> sin (adsr 1/32 1/2 1/8 1/2)))
  (render-tail 1)
  (groove 3/2)
  (do ((i 19 (- i 1)))
      ((= i 4))
    ((retune-edi play 2 i)
     '(1/1 3/2 15/8  5/2)
     '(5/4 3/2 15/8  9/4)
     '(5/6 5/4  3/2  2/1)
     '(5/6 9/8  4/3  2/1)
     '(3/4 9/8  4/3 15/8))))

(define-song "coins-5d23"
  (let ((play (retune-edi play 5 23)))
    (root 220)
    (tempo 45)
    (voice (pipe> triangle
		  (detune 1/4)
		  (adsr 1/9 1 0 0)))
    (groove 7/8 7/8
	    21/16 7/16
	    7/8 7/16 7/16
	    21/16 7/16)
    (play 1 3/4
	  5/6 5/4
	  5/6 3/4 5/6
	  1 15/16)
    (rewind)
    (voice (coins-snare))
    (groove 7/16)
    (play #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1
	  #f 1 #f 1)
    (rewind)
    (voice (pipe> sin
		  (adsr 0 1/2 0 0)
		  (volume 1/24)))
    (groove 7/16)
    (play '(3/2 15/8) 3 '(3/2 15/8) 5/2
	  '(5/3 2/1) 3 '(5/3 2/1) 5/2
	  '(5/3 2/1) 3 '(5/3 9/4) 5/2
	  '(3/2 15/8) 3 '(3/2 15/8) 5/2)
    (rewind)
    (voice (pipe> sin
		  (fm (pipe> sin
			     (volume 1/128)
			     (adsr 0 1 0 0)
			     (offset 1)))
		  (adsr 0 1/2 0 0)
		  (volume 1/8)))
    (groove 1/3 1/3 1/3 5/16 1/8 5/16)
    (play 1 3/2 1 3/2 2 15/8)
    (play 5/4 5/3 5/4 5/3 2 15/8)
    (play 5/4 5/3 5/4 5/3 2 15/8)
    (play 1 3/2 1 3/2 2 15/8)
    (rewind)
    (voice (coins-kick))
    (groove 7/16)
    (play 1 1 1 1
	  1 1 1 1
	  1 1 1 1
	  1 1 1 1)))

(define-song "rhythm-11-9d5"
  (let ((play (retune-edi play 11/9 5)))
    (root 230)
    (tempo 120)
    (voice rhythm-pluck)
    (groove 2/3 1/3 1/3)
    (play 1 2 2
	  1 2 2
	  5/4 #f 5/2
	  5/4 #f 5/2
	  3/2 #f 3
	  3/2 #f 3
	  5/3 #f 10/3
	  5/3 #f 10/3
	  4/3 #f 8/3
	  4/3 #f 8/3
	  5/4 #f 5/2
	  5/4 #f 5/2
	  5/4 #f 5/2
	  5/4 #f 5/2
	  #f #f #f
	  #f #f #f)
    (groove 1/3 1/3 6/3)
    (play 1 9/8 5/3
	  9/8 5/4 9/4)
    (groove 1/3)
    (play 5/4 4/3 5/2 8/3 5/2 #f 5/2 #f)
    (groove 1/3 1/3 6/3)
    (play 8/3 5/2 2/1
	  5/2 9/4 15/8
	  9/4 2/1 3/2)
    (groove 1/3)
    (play 5/3 3/2 4/3
	  #f 5/4 #f
	  9/8 #f 5/4
	  #f 4/3 #f
	  5/4 #f 9/8)
    (rewind)
    (voice rhythm-kick)
    (groove 1 1/3 2/3 2/3)
    (do ((i 0 (+ 1 i)))
        ((= i 16))
      (play 1 #f 1 #f))
    (rewind)
    (voice rhythm-snare)
    (groove 1 1/3 2/3 1/3 1/3)
    (do ((i 0 (+ 1 i)))
        ((= i 16))
      (play #f 1 #f 1 #f))
    (rewind)
    (voice rhythm-pad)
    (groove 2/3)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (play #f '(1 3/4 15/8 5/2)
	    #f '(1 3/4 15/8 5/2)
	    2 '(1 3/4 7/4 5/2)
	    2 '(1 3/4 7/4 5/2)
	    5/3 '(1 3/4 5/3 5/2)
	    #f '(1 3/4 5/3 5/2)
	    #f '(1 3/4 5/3 5/2)
	    #f '(1 3/2 5/3 5/2))
      (play #f '(5/6 5/4 3/2 2/1)
	    #f '(5/6 5/4 3/2 2/1)
	    9/4 '(5/6 9/8 4/3 2/1)
	    9/4 '(5/6 9/8 4/3 2/1)
	    5/2 '(3/4 9/8 4/3 15/8)
	    #f '(3/4 9/8 4/3 15/8)
	    #f '(3/4 9/8 4/3 15/8)
	    #f '(3/4 9/8 4/3 15/8)))
    (rewind)
    (voice (pipe> sin
		  (adsr 1/32 1/8 1/128 0)
		  (detune 4)
		  (volume 1/20)))
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (groove 20/3)
      (play #f)
      (groove 1/3)
      (play 1 9/8 5/4
	    4/3 3/2 5/3
	    3/2 #f 3/2
	    #f 3/2 #f)
      (groove 20/3)
      (play #f)
      (groove 1/3)
      (play 2 15/8 5/3
	    3/2 4/3 5/4
	    5/4 #f 5/4
	    #f 5/4 #f))
    (rewind)
    (voice (pipe> triangle
		  (adsr 1/2 1/2 0 0)
		  (detune 1/4)
		  (volume 1/6)))
    (groove 1 1/3 2/3 2/3)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (play 1 3/2 1 1
	    1 3/2 1 1
	    3/4 1 3/4 3/4
	    3/4 1 3/4 3/4
	    3/4 3/2 3/4 3/4
	    3/2 3/4 3/2 3/4
	    9/8 9/4 9/8 9/8
	    9/4 9/8 9/4 9/8))))

(define-song "smokey-water"
  (root 320)
  (tempo 170)
  (voice smokey-pluck)
  (render-tail 1)
  (do ((i 0 (+ 1 i)))
      ((= i 2))
    (groove 1/2)
    (play 1 1 6/5 1 4/3 1 7/5 4/3
	  1 1 6/5 1 4/3 1 7/5 4/3)
    (groove 1/2 1 2)
    (play 6/5 6/5 10/9 10/9 10/9)
    (groove 3/2 1)
    (play 1 7/8))
  (do ((i 0 (+ 1 i)))
      ((= i 2))
    (groove 1/2 1/2 2)
    (play 1 7/8 1
	  6/5 10/9 6/5
	  3/2 7/5 3/2
	  1 7/8 1
	  7/6 9/8 7/6
	  8/5 3/2 7/5)
    (groove 1)
    (play 3/2 8/5 9/5))
  (groove 1 3)
  (play 1 1))

(define-song "flippies"
  (root 170)
  (tempo 90)
  (voice
   (pipe> sin
	  (fm (pipe> sin
		     (hz 3)
		     (volume 1/100)
		     (adsr 1 0 1 1/2)
		     (offset 1)))
	  (am (pipe> sin
		     (hz 3)
		     (volume 1/81)
		     (adsr 1/2 1/2 0 0)
		     (offset 1/2)))
	  (adsr 1/16 2 0 1)))
  (render-tail 2)
  (groove 1/2)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (play 1 3/2 15/8 3/2
	  2 15/8 5/3 3/2)
    (play 1 3/2 15/8 3/2
	  2 15/8 5/3 3/2)
    (play 5/6 5/4 4/3 5/4
	  3/4 5/6 4/3 5/4
	  5/6 5/4 4/3 5/4
	  3/2 4/3 5/4 5/6))
  (rewind)
  (voice
   (pipe> sin
	  (adsr 1 1/2 1/8 1/2)
	  (detune 1/8)
	  (volume 1/4)
	  (am (pipe> sin
		     (hz (* 1/4 (/ (tempo) 60)))
		     (volume 1)
		     (adsr 1/2 1/2 1/2 1/2)
		     (offset 1/2)))
	  (fm (pipe> sin
		     (detune 3/8)
		     (volume 1/4)
		     (offset 2/3)
		     (adsr 1/32 1/16 1/81 1/2)))))
  (render-tail 1/2)
  (groove 16)
  (play 1 3/4 1 3/4)
  (groove 8)
  (play 1))

(define-song "bustifer-j-christ"
  (root 130)
  (tempo 60)
  (voice bustifer-pluck)
  (render-tail 3)
  (groove 2 1 1/4 1/4 1/4 1/4
	  2 2/5 2/5 2/5 2/5 2/5)
  (apply play (pipe>> (iota 48)
		      (map (lambda>> (expt (inexact (expt 11/9 4)))))
		      (map (lambda> (interval-reduce 3)))
		      (map (lambda (i) (list i (* i 11/9) (* i 11/9 11/9 11/9))))))
  (voice bustifer-bass)
  (rewind)
  (do ((i 0 (+ 1 i)))
      ((= i 4))
    (groove 1/4)
    (play 1 #f 1 #f
	  1 #f 1 #f
	  1 #f 1 #f
	  1 #f 1 #f
	  1 #f 1 2
	  1 #f 1 #f)
    (groove 2/5)
    (play 2 3/2 4/3 2 3/2)))

(define-song "dark-chords"
  (root 7)
  (tempo 60)

  (voice dark-chords)
  (render-tail 4)
  (groove 1/8)
  (do ((i 0 (+ 1 i)))
      ((= i 2))
    (play 19 21
	  19 21 28
	  19 21 19
	  19 21
	  19 21 28
	  19 21 19
	  19 21 14 35/2
	  19 21 14 35/2
	  19 21 14 35/2
	  19 21 14 35/2
	  19 14 35/2 49/2
	  19 14 35/2 49/2
	  19 14 49/2 63/4
	  19 14 49/2 63/4
	  19 49/2 63/4 70/2
	  19 49/2 63/4 77/2
	  19 63/4 77/4 91/2
	  19 23 29 31 46
	  19 28 38 57 69))
  (play 70 77 76))

(define-song "wowie"
  (root 200)

  ;; arp
  (when #t
    (voice (pipe> sin
		  (adsr 1/32 31/32 0 0)
		  (volume 1/4)))
    (render-tail 1)
    (groove 1/4)
    (do ((i 0 (+ i 1)))
        ((= i 2))
      (do ((i 0 (+ i 1)))
	  ((= i 4))
        (play 1 3/2 3/2 1 3/2 3/2 3/2 3/2)
        (modulate 2/3)
        (play 1 3/2 3/2 1 3/2 3/2 3/2 3/2)
        (modulate 3/2))
      (play 1 4/3 4/3 3/2 5/3 5/3 7/4 7/4)
      (play 7/4 9/4 9/4 8/3 3 3 7/2 7/2)))
  
  ;; pad
  (when #t
    (rewind)
    (voice wowie-pad)
    (render-tail 1)
    (groove 2)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (do ((i 0 (+ 1 i)))
	  ((= i 4))
        (play '(1 3/2 7/4)
	      '(1 4/3 5/3)))
      (play '(3/2 7/4 9/4)
	    '(3/2 15/8 9/4))))

  ;; bass
  (when #t
    (rewind)
    (voice wowie-bass)
    (groove 1/4 5/4 3/2 1/4 1/4 1/4 1/4)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (do ((i 0 (+ 1 i)))
	  ((= i 2))
        (play 1 1 3/2 1 1 1 1
	      1 1 4/3 1 2 1 3/2))
      (play 3/2 3/2 4/3 3/2 3/2 3/2 3))
    (groove 1)
    (play 1))

  ;; hat
  (when #t
    (rewind)
    (voice wowie-hat)
    (groove 1/4)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (do ((i 0 (+ 1 i)))
	  ((= i 16))
        (play #f 1 #f 1))
      (play #f #f)))
  
  ;; snare
  (when #t
    (rewind)
    (voice wowie-snare)
    (groove 1/4)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (do ((i 0 (+ 1 i)))
	  ((= i 8))
        (play #f #f #f #f
	      1 #f #f #f))
      (play #f #f #f 1)))

  ;; kick
  (when #t
    (rewind)
    (voice wowie-kick)
    (groove 1/4)
    (do ((i 0 (+ 1 i)))
        ((= i 2))
      (do ((i 0 (+ 1 i)))
	  ((= i 32))
        (play 1 #f))
      (play 1 1 1 1))))

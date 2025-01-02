(library (pcm)
  (export pipe> pipe>> lambda> lambda>>
          while repeat
          flat-map cross-join
          harmonics unison rhythm
          quick-reduce interval-reduce interval-balance
          count-factor factorize prime? factors
          radius-of-tolerance fjs-note-names fractran
          fifth-shift formal-comma fjs-accidental
          interval->fjs fjs->interval edi-patent-val
          cubic vector-index vector-fold-left
          vector-normalize ensure-list flatten
          retune-edi-note snap-note-to-scale
          snap-to-scale retune-edi fjs-print
          reduce-intervals index-edi index-note-to-scale
          index-scale mos-pattern make-cyclic-list
          arpeggiate cents arpeggio
          rank-2-temperament merge-scale-offset
          rotate-scale snap-to-chroma
          harmonic-series-segment edi
          sample-rate roughly-pi render-tail
          beat-position beat-counter tape-tail
          tape-head rewind voice
          tempo groove beat-length root modulate
          square sawtooth triangle noise
          offset volume adsr detune hz add sub
          distort
          am fm pm rm play rest
          define-song song-database render-pcm16 render-musicxml)
  (import (chezscheme))

;;; syntax

  (define-syntax pipe>
    (syntax-rules (tee> tee>> pipe>> pipe< pipe<<)
      ((pipe> x) x)
      ((pipe> x (pipe< f (p ...) ...) g ...)
       (pipe> (let (($x x))
                (f (pipe> $x (p ...)) ...))
              g ...))
      ((pipe> x (pipe<< f (p ...) ...) g ...)
       (pipe> (let (($x x))
                (f (pipe>> $x (p ...)) ...))
              g ...))
      ((pipe> x (tee> p ...) g ...)
       (pipe> (let (($x x))
                (pipe> $x p ...)
                $x)
              g ...))
      ((pipe> x (tee>> p ...) g ...)
       (pipe> (let (($x x))
                (pipe>> $x p ...)
                $x)
              g ...))
      ((pipe> x (pipe>> p ...) g ...)
       (pipe> (pipe>> x p ...) g ...))
      ((pipe> x (f y ...) g ...)
       (pipe> (f x y ...) g ...))))

  (define-syntax pipe>>
    (syntax-rules (tee> tee>> pipe> pipe< pipe<<)
      ((pipe>> y) y)
      ((pipe>> x (pipe< f (p ...) ...) g ...)
       (pipe>> (let (($x x))
                 (f (pipe> $x (p ...)) ...))
               g ...))
      ((pipe>> x (pipe<< f (p ...) ...) g ...)
       (pipe>> (let (($x x))
                 (f (pipe>> $x (p ...)) ...))
               g ...))
      ((pipe>> x (tee> p ...) g ...)
       (pipe>> (let (($x x))
                 (pipe> $x p ...)
                 $x)
               g ...))
      ((pipe>> y (tee>> p ...) g ...)
       (pipe>> (let (($y y))
                 (pipe>> $y p ...)
                 $y)
               g ...))
      ((pipe>> y (pipe> p ...) g ...)
       (pipe>> (pipe> y p ...) g ...))
      ((pipe>> y (f x ...) g ...)
       (pipe>> (f x ... y) g ...))))

  (define-syntax lambda>
    (syntax-rules ()
      ((lambda> body ...)
       (lambda (x) (pipe> x body ...)))))

  (define-syntax lambda>>
    (syntax-rules ()
      ((lambda>> body ...)
       (lambda (y) (pipe>> y body ...)))))

  (define-syntax while
    (syntax-rules ()
      ((while condition body ...)
       (let loop ()
         (when condition body ... (loop))))))

  (define-syntax repeat
    (syntax-rules ()
      ((repeat n body ...)
       (let (($n n))
         (do ((i 0 (+ i 1)))
             ((= i $n))
           body ...)))))

;;; utility functions

  (define (flat-map selector source)
    (fold-left append '() (map selector source)))

  (define (cross-join primary-source . secondary-sources)
    (map reverse
         (fold-left (lambda (rows next-source)
                      (flat-map (lambda (row)
                                  (map (lambda (next-column)
                                         (cons next-column row))
                                       next-source))
                                rows))
                    (map list primary-source)
                    secondary-sources)))

  (define (harmonics . ratios)
    (volume (pipe>> ratios
                    (map (lambda (r) (detune sin r)))
                    (apply add))
            (/ (length ratios))))

  (define (unison op . ratios)
    (volume (pipe>> ratios
                    (map (lambda (r) (detune op r)))
                    (apply add))
            (/ (length ratios))))

  (define (rhythm play . voices)
    (for-each (lambda (v)
                (if v
                    (parameterize ((voice v))
                      (play 1))
                    (play #f)))
              voices))

  (define (rest duration)
    (parameterize ((groove duration))
      (play #f)))

  (define (quick-reduce a r)
    (let ((n (log a r)))
      (expt r (- n (floor n)))))

  (define (interval-reduce interval equivalence)
    "Reduce the given interval to be within 1/1 and the equivalence"
    (cond ((zero? interval) #f)
          ((negative? equivalence)
	   (interval-reduce interval (- equivalence)))
	  ((< equivalence 1)
	   (interval-reduce interval (/ equivalence)))
	  ((< interval 1)
	   (interval-reduce (* interval equivalence) equivalence))
	  ((>= interval equivalence)
	   (interval-reduce (/ interval equivalence) equivalence))
	  (else interval)))

  (define (interval-balance interval)
    "Ensure the given interval is smaller than or equal to 1"
    (if (>= interval 1)
        interval
        (/ interval)))

  (define (count-factor interval factor)
    "Count the times factor appears in interval as an exponent"
    (let loop ((n 0) (i interval))
      (if (zero? (modulo i factor))
	  (loop (+ 1 n) (/ i factor))
	  n)))

  (define (factorize interval factors)
    "Given a list of factors, return the list of their counts in interval"
    (and (not (zero? interval))
         (let ((n (numerator interval))
	       (d (denominator interval)))
           (map (lambda (factor)
	          (- (count-factor n factor)
	             (count-factor d factor)))
	        factors))))

  (define (fractran state . program)
    (let run ((state state) (this-pass program))
      (if (or (zero? state)
              (null? this-pass))
          state
          (let ((next-state (* (car this-pass) state)))
            (if (integer? next-state)
                (run next-state program)
                (run state (cdr this-pass)))))))

  (define (prime? n)
    "Determine whether n is prime or composite"
    (let loop ((i 2))
      (cond ((> i (sqrt n)) #t)
            ((zero? (mod n i)) #f)
            (else (loop (+ i 1))))))

  (define (factors n)
    "Get the list of prime factors of n"
    (pipe>> (iota n)
            (map add1)
            (filter (lambda (i)
                      (and (prime? i)
                           (zero? (mod n i)))))))

  (define radius-of-tolerance
    (make-parameter (sqrt 256/243)))

  (define fjs-note-names
    (make-parameter '#(F C G D A E B)))

  (define (fifth-shift interval)
    "Find the 3-limit height of interval, in 3/2s"
    (let ((p (interval-reduce interval 2)))
      (let loop ((k 0))
        (cond ((< (interval-balance (/ p (interval-reduce (expt 3 k) 2)))
		  (radius-of-tolerance))
	       k)
	      ((zero? k) (loop 1))
	      ((negative? k) (loop (+ 1 (- k))))
	      (else (loop (- k)))))))

  (define (formal-comma p k)
    "Get the FJS comma for interval p with fifth-shift k"
    (let ((comma (interval-reduce (/ p (expt 3 k)) 2)))
      (if (< comma (sqrt 2))
	  comma
	  (/ comma 2))))

  (define (fjs-accidental k)
    "Get the number of accidentals for the given fifth-shift k"
    (floor (/ k (vector-length (fjs-note-names)))))

  (define (interval->fjs p)
    "Convert the interval p to FJS notation"
    (let ((k (fifth-shift p)))
      (list (vector-ref (fjs-note-names)
		        (modulo (+ 1 k)
			        (vector-length (fjs-note-names))))
	    (fjs-accidental k)
	    (formal-comma p k)
            p)))

  (define (fjs->interval name accidental comma)
    "Convert the interval given by (name accidental comma) from FJS to a number"
    (let* ((i (- (vector-index (fjs-note-names) name) 1))
	   (k (+ i (* accidental (vector-length (fjs-note-names))))))
      (* comma (interval-reduce (expt 3 k) 2))))

  (define (edi-patent-val base number-of-equal-divisions interval)
    "Get the patent val for the given INTERVAL in the temperament
given by dividing BASE into NUMBER-OF-EQUAL-DIVISIONS"
    (if (zero? interval) 0
        (log interval (expt base (/ number-of-equal-divisions)))))

  (define (cubic unit)
    "Convert a linear unit (0, 1) to a cubic unit (0, 1)"
    (- 1 (expt (- 1 unit) 3)))

  (define (vector-index vector item)
    "Find the index where an item appears in a vector"
    (let loop ((i 0))
      (cond ((>= i (vector-length vector)) #f)
	    ((eq? item (vector-ref vector i)) i)
	    (else (loop (+ 1 i))))))

  (define (vector-fold-left kons knil . vectors)
    (let ((n (apply min (map vector-length vectors))))
      (do ((i 0 (+ 1 i))
	   (result knil
		   (apply kons result (map (lambda (v) (vector-ref v i)) vectors))))
	  ((= i n) result))))

  (define (vector-normalize n vector)
    "Normalize numbers in vector so they're between 0 and ±n"
    (let ((amplitude-max
	   (vector-fold-left (lambda (a b) (max (abs a) (abs b)))
			     0
			     vector)))
      (if (zero? amplitude-max)
	  vector
	  (vector-map (lambda (x) (* n (/ x 2 amplitude-max)))
		      vector))))

  (define (ensure-list item)
    "If item is a list, return it. Otherwise, wrap it in a list"
    (if (list? item) item (list item)))

  (define (flatten list-of-lists)
    (fold-left (lambda (flat list-or-item)
                 (if (list? list-or-item)
                     (append flat (flatten list-or-item))
                     (append flat (list list-or-item))))
               '()
               list-of-lists))

  (define (retune-edi-note note b n)
    (expt b (/ (round (edi-patent-val b n note)) n)))

  (define (snap-note-to-scale note scale)
    (let* ((scale-length (vector-length scale))
           (equave (vector-ref scale (sub1 scale-length)))
           (index (* (log note equave) scale-length))
           (lower (index-note-to-scale (exact (floor index)) scale))
           (upper (index-note-to-scale (exact (ceiling index)) scale)))
      (if (< (interval-balance (/ note lower))
             (interval-balance (/ note upper)))
          lower
          upper)))

  (define (snap-to-scale play scale)
    (lambda notes-or-chords
      (pipe>> notes-or-chords
              (map ensure-list)
              (map (lambda>>
                    (map (lambda (note)
                           (if (number? note)
                               (snap-note-to-scale note scale)
                               note)))))
              (apply play))))

  (define (retune-edi play b n)
    "Wrap the play function, retuning values to fit into the EDI given by base b and number of divisions n"
    (lambda notes-or-chords
      (pipe>> notes-or-chords
	      (map ensure-list)
	      (map (lambda>>
	            (map (lambda (note)
		           (if (number? note)
                               (retune-edi-note note b n)
			       note)))))
	      (apply play))))

  (define (fjs-print play)
    (lambda chords
      (for-each
       (lambda (chord)
         (pretty-print
          (map (lambda (note)
                 (if (number? note)
                     (interval->fjs note)
                     note))
               (ensure-list chord))))
       chords)
      (newline)
      (for-each play chords)))

  (define (reduce-intervals play equave)
    "Wrap the play function, reducing values by the equave"
    (lambda notes-or-chords
      (pipe>> notes-or-chords
	      (map ensure-list)
	      (map (lambda>>
	            (map (lambda (note)
		           (if (number? note)
			       (quick-reduce note equave)
			       note)))))
	      (apply play))))

  (define (index-edi play b n)
    "Wrap the play function, indexing values into the EDI given by base b and number of divisions n"
    (lambda notes-or-chords
      (pipe>> notes-or-chords
	      (map ensure-list)
	      (map (lambda>>
	            (map (lambda (note)
		           (if (number? note)
			       (expt b (/ note n))
			       note)))))
	      (apply play))))

  (define (index-note-to-scale index scale)
    (let* ((length (vector-length scale))
           (equave (vector-ref scale (- length 1))))
      (let loop ((index index)
                 (note 1))
        (cond ((not (number? index)) index)
              ((negative? index)
               (loop (+ index length -1)
                     (/ note equave)))
              ((>= index length)
               (loop (- index length -1)
                     (* note equave)))
              (else
               (* note (vector-ref scale index)))))))

  (define (index-scale play scale)
    "Wrap the play function, indexing values into the given scale vector"
    (lambda notes-or-chords
      (pipe>> notes-or-chords
              (map ensure-list)
              (map (lambda>>
                    (map (lambda (note)
                           (index-note-to-scale note scale)))))
              (apply play))))

  (define mos-pattern
    (letrec ((make-pattern
              (lambda (a-step large b-step small)
                (let* ((count-excess (div a-step b-step))
                       (remaining-excess (- a-step count-excess))
                       (pattern (let loop ((p (list small)) (i count-excess))
                                  (if (zero? i) p
                                      (loop (cons large p) (- i 1))))))
                  (cond ((= 1 b-step)
                         pattern)
                        (else
                         (append (make-pattern remaining-excess
                                               large
                                               (- b-step 1)
                                               small)
                                 pattern)))))))
      (lambda (a-step b-step)
        (if (> a-step b-step)
            (make-pattern a-step 'L b-step 's)
            (make-pattern b-step 's a-step 'L)))))
  #;
  (define (edi-mos-scale equave step-size step-hardness)
  (let* ((number-of-divisions (+ (* (numerator step-size)
  (numerator step-hardness))
  (* (denominator step-size)
  (denominator step-hardness))))
  (scale (make-vector (+ (numerator step-size)
  (denominator step-size)
  1)))
  (scale-length (vector-length scale)))
  (vector-set! scale 0 1)
  (vector-set! scale (- scale-length 1) equave)
  ;; TODO fill out the remaining steps according to MOS pattern
  (let loop ((i 1))
  (cond ((= i scale-length)
  scale)
  (else ())))))

  (define (make-cyclic-list list)
    (let ((head (list-copy list)))
      (let loop ((cell head))
        (cond ((null? (cdr cell))
               (set-cdr! cell head)
               head)
              (else
               (loop (cdr cell)))))))

  (define (arpeggiate play subdivisions)
    (lambda chords
      (for-each (lambda (chord)
                  (let ((old-groove (groove))
                        (old-beat-counter (beat-counter)))
                    (groove (/ (beat-length) subdivisions))
                    (do ((i 0 (+ i 1))
                         (chord (make-cyclic-list (ensure-list chord))
                                (cdr chord)))
                        ((= i subdivisions))
                      (play (car chord)))
                    (groove old-groove)
                    (beat-counter (+ 1 old-beat-counter))))
                chords)))

  (define (cents ratio)
    (* 1200 (log ratio 2)))

  (define (arpeggio play . notes)
    (let ((old-groove (groove))
          (old-beat-counter (beat-counter)))
      (groove (/ (beat-length)
                 (length notes)))
      (beat-counter old-beat-counter)
      (for-each play notes)
      (groove old-groove)
      (beat-counter (+ 1 old-beat-counter))))

  (define (rank-2-temperament generator period size)
    (let ((scale (make-vector (add1 size))))
      (vector-set! scale size period)
      (do ((i 0 (add1 i)))
          ((= i size)
           (vector-sort! < scale)
           scale)
        (vector-set! scale i (interval-reduce (expt generator i) period)))))

  (define (merge-scale-offset offset scale)
    (let* ((scale-length (vector-length scale))
           (period (vector-ref scale (sub1 scale-length)))
           (new-scale (make-vector (sub1 (* 2 scale-length)))))
      (do ((i 0 (add1 i)))
          ((= i (sub1 scale-length))
           (vector-set! new-scale i period)
           (vector-sort! < new-scale)
           new-scale)
        (let ((old-note (vector-ref scale i)))
          (vector-set! new-scale i old-note)
          (vector-set! new-scale (+ i scale-length)
                       (interval-reduce (* offset old-note)
                                        period))))))

  (define (rotate-scale degree scale)
    (let* ((scale-length (vector-length scale))
           (rotation (index-note-to-scale degree scale))
           (period (vector-ref scale (sub1 scale-length)))
           (new-scale (make-vector scale-length)))
      (do ((i 0 (add1 i)))
          ((= i scale-length)
           (vector-set! new-scale (sub1 i) period)
           (vector-sort! < new-scale)
           new-scale)
        (vector-set! new-scale i
                     (interval-reduce
                      (/ (vector-ref scale i) rotation)
                      period)))))

  (define (snap-to-chroma chroma)
    (lambda (note)
      (expt chroma (round (log note chroma)))))

  (define (harmonic-series-segment start end)
    (list->vector
     (map (lambda (n) (/ (+ n start) start))
          (iota (+ 1 (- end start))))))

  (define (edi base number-of-divisions)
    (list->vector
     (map (lambda (n) (expt base (/ n number-of-divisions)))
          (iota (add1 number-of-divisions)))))

;;; constants and parameters

  (define sample-rate
    (make-parameter 48000))

  (define roughly-pi 355/113)

  (define render-tail
    (make-parameter 0))

  (define beat-position
    (make-parameter 0))

  (define beat-counter
    (make-parameter 0))

  (define tape-tail
    (make-parameter 0))

  (define tape-head
    (make-parameter
     0 (lambda (new)
         (tape-tail (max (tape-tail) new))
         new)))

  (define rewind
    (case-lambda (()
		  (tape-head 0)
		  (beat-counter 0))
	         ((beats)
		  (tape-head
		   (max 0 (- (tape-head)
			     (* (sample-rate) beats (/ 60 (tempo))))))
		  (beat-counter 0))))

  (define voice
    (make-parameter #f))

  (define tape-buffer
    (make-parameter #f))

  (define (write-sample sample)
    (let ((buffer (tape-buffer)))
      (and buffer
           (let ((tape-pos (exact (floor (tape-head))))
                 (old-length (vector-length buffer)))
             (when (>= tape-pos old-length)
               (set! buffer
                     (let ((new-buffer (make-vector (* 2 tape-pos) 0)))
                       (do ((i 0 (+ i 1)))
                           ((>= i old-length) new-buffer)
                         (vector-set! new-buffer i (vector-ref buffer i)))))
               (tape-buffer buffer))
             (vector-set! buffer
                          tape-pos
                          (+ sample (vector-ref buffer tape-pos)))))))

  (define tempo
    (make-parameter 60))

  (define groove
    (let ((current '(1)))
      (case-lambda (() current)
		   (new (beat-counter 0)
		        (set! current (flatten (ensure-list new)))))))

  (define (beat-length)
    (* (/ 60 (tempo))
       (list-ref (groove)
	         (mod (beat-counter)
		      (length (groove))))))

  (define root
    (make-parameter 440))

  (define (modulate n)
    (root (* (root) n)))

;;; oscillator functions

  (define (square i)
    (* 2 (floor (+ 1/2 (sin i)))))

  (define (sawtooth i)
    (- 1 (mod (/ i roughly-pi) 2.0)))

  (define (triangle i)
    (- (* 2 (abs (sawtooth i))) 1))

  (define noise
    (let ((buffer (make-vector (sample-rate) 0)))
      (do ((i 0 (+ i 1))
	   (s 0 (+ s (random 1.0))))
	  ((>= i (vector-length buffer)))
        (vector-set! buffer i (sin s)))
      (lambda (i)
        (vector-ref buffer (mod (exact (round (abs i)))
			        (vector-length buffer))))))

;;; oscillator combinators and modifiers

  (define (offset carrier . numbers)
    (lambda (i)
      (apply + (carrier i) numbers)))

  (define (volume carrier . modifiers)
    (lambda (i)
      (apply * (carrier i) modifiers)))

  (define (adsr carrier attack decay sustain release)
    (lambda (i)
      (* (carrier i)
         (cond ((< (beat-position) attack)
	        (cubic (/ (beat-position)
			  attack)))
	       ((< (beat-position) (+ attack decay))
	        (- 1 (* (- 1 sustain)
		        (cubic (/ (- (beat-position) attack)
				  decay)))))
	       ((< (beat-position) 1)
	        sustain)
	       ((< (beat-position) (+ 1 release))
	        (* sustain
		   (- 1 (cubic (/ (- (beat-position) 1)
				  release)))))
	       (else 0)))))

  (define (detune carrier . modifiers)
    (lambda (i)
      (carrier (apply * i modifiers))))

  (define (hz carrier freq)
    (lambda (_)
      (carrier (* freq 2 roughly-pi
		  (beat-position)))))

  (define (add carrier . operators)
    (lambda (i)
      (fold-left (lambda (s o) (+ s (o i)))
	         (carrier i)
	         operators)))

  (define (sub carrier . operators)
    (lambda (i)
      (fold-left (lambda (s o) (- s (o i)))
	         (carrier i)
	         operators)))

  (define (am carrier . operators)
    (lambda (i)
      (fold-left (lambda (s o) (* s (o i)))
	         (carrier i)
	         operators)))

  (define (fm carrier . operators)
    (lambda (i)
      (carrier (fold-left (lambda (s o) (* s (o i)))
			  i
			  operators))))

  (define (pm carrier . operators)
    (lambda (i)
      (carrier (fold-left (lambda (s o) (+ s (o i)))
			  i
			  operators))))

  (define (rm carrier . operators)
    (lambda (i)
      (fold-left (lambda (s o)
		   (if (negative? s)
		       (max s (- (abs (o i))))
		       (min s (abs (o i)))))
	         (carrier i)
	         operators)))

  (define (distort carrier . distortions)
    (lambda (i)
      (fold-left (lambda (signal distortion)
                   (distortion signal))
                 (carrier i)
                 distortions)))

;;; rendering

  (define (play-to-tape . notes-or-chords)
    (for-each (lambda (note-or-chord)
	        (let* ((tape-start (tape-head))
		       (beat-samples
		        (* (sample-rate) (beat-length)))
		       (tail-samples
		        (* beat-samples (+ 1 (render-tail)))))
		  (for-each
                   (let ((v (voice))
                         (r (root))
                         (s (sample-rate)))
                     (if (procedure? v)
                         (lambda (note)
                           (do ((sample 0 (+ 1 sample)))
		               ((>= sample tail-samples))
		             (beat-position (/ sample beat-samples))
		             (tape-head (+ tape-start sample))
                             (write-sample (v (* note r 2 roughly-pi (/ sample s))))))
                         (lambda (note)
                           (let ((tail (- tail-samples 1)))
                             (beat-position (/ (- tail-samples 1) beat-samples))
                             (tape-head (+ tape-start (- tail-samples 1)))))))
		   (filter
		    number?
		    (if (list? note-or-chord)
		        note-or-chord
		        (list note-or-chord))))
		  (beat-counter (+ 1 (beat-counter)))
		  (tape-head (+ tape-start beat-samples))))
	      notes-or-chords))

  (define (fjs->musicxml fjs)
    (let ((name (car fjs))
          (accidental (cadr fjs))
          (formal-comma (caddr fjs))
          (original-note (cadddr fjs)))
      `(pitch (step ,name)
              (alter ,(* (if (negative? accidental) -1 1)
                         (* 1/100 (cents (expt formal-comma (abs accidental))))))
              (octave ,(+ 4 (exact (floor (log original-note 2))))))))

  (define (musicxml->string xml)
    (cond ((list? xml)
           (format "<~a>~a</~a>"
                   (car xml)
                   (fold-left string-append "" (map musicxml->string (cdr xml)))
                   (car xml)))
          (else
           (format "~a" xml))))

  (define musicxml-output-port
    (make-parameter #f))

  (define (beat-length->musicxml)
    `(duration ,(inexact (beat-length))))

  (define (play-to-musicxml . notes-or-chords)
    (let ((active-voice #f))
      (for-each (lambda (chord)
                  (let ((notes (filter number? chord)))
                    (if (null? notes)
                        (format (musicxml-output-port) "<rest />")
                        (begin (format (musicxml-output-port) "~a"
                                       (musicxml->string
                                        (list 'note
                                              (beat-length->musicxml)
                                              (fjs->musicxml (interval->fjs (car notes))))))
                               (for-each (lambda (note)
                                           (format (musicxml-output-port) "~a "
                                                   (musicxml->string
                                                    (list 'note
                                                          (beat-length->musicxml)
                                                          (cons 'pitch
                                                                (cons '(chord)
                                                                      (cdr (fjs->musicxml
                                                                            (interval->fjs note)))))))))
                                         (cdr notes)))))
                  (beat-counter (+ 1 (beat-counter))))
                (map ensure-list notes-or-chords))))

  (define play-mode
    (make-parameter play-to-tape))

  (define (play . notes-or-chords)
    (apply (play-mode) notes-or-chords))

  (define (reset-and-run-song thunk)
    (tape-head 0)
    (tape-tail 0)
    (render-tail 0)
    (groove 1)
    (tempo 60)
    (root 440)
    (voice sin)
    (thunk))

  ;;; songs

  (define song-database
    (make-hashtable string-hash string=?))

  (define-syntax define-song
    (syntax-rules ()
      ((song name body ...)
       (hashtable-set!
        song-database
        name
        (lambda () body ...)))))

  (define (render-pcm16 name)
    (format #t "Rendering ~a...~%" name)
    (format #t "Done!~%Output: ~a~%"
            (let ((filename (format "render/~a.pcm16" name))
                  (thunk (hashtable-ref song-database name #f)))
              (parameterize ((tape-buffer (make-vector (* 3 60 (sample-rate)) 0))
                             (play-mode play-to-tape))
                (reset-and-run-song thunk)
                (call-with-port (open-file-output-port filename (file-options no-fail))
                  (lambda (output)
                    (let ((buffer (vector-normalize 2/3 (tape-buffer)))
                          (tail (exact (ceiling (tape-tail)))))
                      (do ((i 0 (+ i 1)))
                          ((= i (min tail (vector-length buffer))))
                        (let ((s (exact (round (* #xFFFF (vector-ref buffer i))))))
                          (put-u8 output (bitwise-and #xFF s))
                          (put-u8 output (bitwise-arithmetic-shift (bitwise-and #xFF00 s) -8))))))))))
    (values))

  (define (render-musicxml name)
    (let ((filename (format "render/~a.xml" name))
          (thunk (hashtable-ref song-database name #f)))
      (parameterize ((play-mode play-to-musicxml)
                     (musicxml-output-port (open-file-output-port filename
                                                                  (file-options no-fail)
                                                                  (buffer-mode none)
                                                                  (native-transcoder))))
        (format (musicxml-output-port)
                "<score-partwise><part><measure>~%")
        (reset-and-run-song thunk)
        (format (musicxml-output-port)
                "~%</measure></part></score-partwise>")
        (close-output-port (musicxml-output-port))))))

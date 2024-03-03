;;; syntax

(define-syntax ->
  (syntax-rules ()
    ((-> x) x)
    ((-> x (f y ...) g ...)
     (-> (f x y ...) g ...))))

(define-syntax ->>
  (syntax-rules ()
    ((->> y) y)
    ((->> y (f x ...) g ...)
     (->> (f x ... y) g ...))))

(define-syntax lambda->
  (syntax-rules ()
    ((lambda-> body ...)
     (lambda (x) (-> x body ...)))))

(define-syntax lambda->>
  (syntax-rules ()
    ((lambda->> body ...)
     (lambda (y) (->> y body ...)))))

;;; utility functions

(define (interval-reduce interval equivalence)
  (cond ((negative? equivalence)
	 (interval-reduce interval (- equivalence)))
	((< equivalence 1)
	 (interval-reduce interval (/ equivalence)))
	((< interval 1)
	 (interval-reduce (* interval equivalence) equivalence))
	((>= interval equivalence)
	 (interval-reduce (/ interval equivalence) equivalence))
	(else interval)))

(define (interval-balance interval)
  (if (>= interval 1)
      interval
      (/ interval)))

(define (count-factor interval factor)
  (let loop ((n 0) (i interval))
    (if (zero? (modulo i factor))
	(loop (+ 1 n) (/ i factor))
	n)))

(define (factorize interval factors)
  (let ((n (numerator interval))
	(d (denominator interval)))
    (map (lambda (factor)
	   (- (count-factor n factor)
	      (count-factor d factor)))
	 factors)))

(define radius-of-tolerance
  (make-parameter 65/63))

(define fjs-note-names
  (make-parameter '#(F C G D A E B)))

(define (fifth-shift interval)
  (let ((p (interval-reduce interval 2)))
    (let loop ((k 0))
      (cond ((< (interval-balance (/ p (interval-reduce (expt 3 k) 2)))
		(radius-of-tolerance))
	     k)
	    ((zero? k) (loop 1))
	    ((negative? k) (loop (+ 1 (- k))))
	    (else (loop (- k)))))))

(define (formal-comma p k)
  (let ((comma (interval-reduce (/ p (expt 3 k)) 2)))
    (if (< comma (sqrt 2))
	comma
	(/ comma 2))))

(define (fjs-accidental k)
  (floor (/ k (vector-length (fjs-note-names)))))

(define (interval->fjs p)
  (let ((k (fifth-shift p)))
    (list (vector-ref (fjs-note-names)
		      (modulo (+ 1 k)
			      (vector-length (fjs-note-names))))
	  (fjs-accidental k)
	  (formal-comma p k))))

(define (fjs->interval name accidental comma)
  (let* ((i (- (vector-index (fjs-note-names) name) 1))
	 (k (+ i (* accidental (vector-length (fjs-note-names))))))
    (* comma (interval-reduce (expt 3 k) 2))))

(define (edi-patent-val base number-of-equal-divisions interval)
  "Get the patent val for the given INTERVAL in the temperament
given by dividing BASE into NUMBER-OF-EQUAL-DIVISIONS"
  (if (zero? interval) 0
      (log interval (expt base (/ number-of-equal-divisions)))))

(define (cubic unit)
  (- 1 (expt (- 1 unit) 3)))

(define (vector-index vector item)
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
  (let ((amplitude-max
	 (vector-fold-left (lambda (a b) (max (abs a) (abs b)))
			   0
			   vector)))
    (if (zero? amplitude-max)
	vector
	(vector-map (lambda (x) (* n (/ x 2 amplitude-max)))
		    vector))))

(define (ensure-list item)
  (if (list? item) item (list item)))

(define (retune-edi play b n)
  (lambda notes-or-chords
    (->> notes-or-chords
	 (map ensure-list)
	 (map (lambda->>
	       (map (lambda (note)
		      (if (number? note)
			  (expt b (/ (round (edi-patent-val b n note)) n))
			  note)))))
	 (apply play))))

(define (index-edi play b n)
  (lambda notes-or-chords
    (->> notes-or-chords
	 (map ensure-list)
	 (map (lambda->>
	       (map (lambda (note)
		      (if (number? note)
			  (expt b (/ note n))
			  note)))))
	 (apply play))))

;;; constants and parameters

(define sample-rate 48000)

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
			   (* sample-rate beats (/ 60 (tempo))))))
		(beat-counter 0))))

(define voice
  (make-parameter #f))

(define sample-writer
  (make-parameter #f))

(define tempo
  (make-parameter 60))

(define groove
  (let ((current '(1)))
    (case-lambda (() current)
		 (new (beat-counter 0)
		      (set! current new)))))

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
  (let ((buffer (make-vector sample-rate 0)))
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
    (fold-left + (carrier i) numbers)))

(define (volume carrier . modifiers)
  (lambda (i)
    (fold-left * (carrier i) modifiers)))

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
    (carrier (fold-left * i modifiers))))

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

;;; rendering

(define (play . notes-or-chords)
  (for-each (lambda (note-or-chord)
	      (let* ((tape-start (tape-head))
		     (beat-samples
		      (* sample-rate (beat-length)))
		     (tail-samples
		      (* beat-samples (+ 1 (render-tail)))))
		(for-each
		 (lambda (note)
		   (do ((sample 0 (+ 1 sample)))
		       ((>= sample tail-samples))
		     (beat-position (/ sample beat-samples))
		     (tape-head (+ tape-start sample))
		     (when (voice)
		       (let ((sample-value
			      ((voice)
			       (* note (root) 2 roughly-pi
				  (/ sample sample-rate)))))
			 (when (sample-writer)
			   ((sample-writer) sample-value))))))
		 (filter
		  number?
		  (if (list? note-or-chord)
		      note-or-chord
		      (list note-or-chord))))
		(beat-counter (+ 1 (beat-counter)))
		(tape-head (+ tape-start beat-samples))))
	    notes-or-chords))

(define (render thunk)
  (tape-head 0)
  (tape-tail 0)
  (render-tail 0)
  (groove 1)
  (tempo 60)
  (root 440)
  (voice #f)
  (thunk))

(define (render-to-file filename thunk)
  (sample-writer #f)
  (render thunk)
  (let ((buffer (make-vector (+ 1 (exact (ceiling (tape-tail)))) 0)))
    (sample-writer
     (lambda (sample)
       (let* ((tape-pos (exact (floor (tape-head))))
	      (existing-sample (vector-ref buffer tape-pos)))
	 (vector-set! buffer tape-pos (+ existing-sample sample)))))
    (render thunk)
    (call-with-port (open-file-output-port filename (file-options no-fail))
      (lambda (output)
	(vector-for-each
	 (lambda (sample)
	   (let ((s (exact (round (* #xFFFF sample)))))
	     (put-u8 output (bitwise-and #xFF s))
	     (put-u8 output (bitwise-arithmetic-shift (bitwise-and #xFF00 s) -8))))
	 (vector-normalize 2/3 buffer))))
    filename))

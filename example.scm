(define (retuned-play b n)
  (lambda notes-or-chords
    (->> notes-or-chords
	 (map (lambda (item) (if (list? item) item (list item))))
	 (map (lambda->>
	       (map (lambda (note)
		      (if (number? note)
			  (expt b (/ (round (patent-val b n note)) n))
			  note)))))
	 (apply play))))

(render-to-file
 "greensleeves-Dm-2d15.pcm16"
 (let ((play (retuned-play 2 15)))
   (lambda ()
     (root 220)
     (tempo 60)
     ;; melody
     (voice (-> sin (adsr 1/32 15/16 0 0)))
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
	   '(9/10 10/9 4/3)))))

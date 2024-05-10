(define over-7 '#(1 8/7 9/7 10/7 11/7 12/7 13/7 14/7))
(define over-10 '#(10/10 11/10 12/10 13/10 14/10 15/10 16/10 17/10 18/10 19/10 20/10))
(define 5-limit-major '#(1 9/8 5/4  4/3  3/2  5/3 15/8 2/1))
(define 5-limit-major<3/2>
  '#(1 9/8 5/4 4/3 3/2))
(define (edi-scale-pattern equave . pattern)
  (let* ((scale-length (length pattern))
         (scale (make-vector (add1 scale-length)))
         (total (fold-left + 0 pattern)))
    (let loop ((pat pattern)
               (n 0)
               (i 0))
      (vector-set! scale i
                     (expt equave (/ n total)))
      (if (pair? pat)
          (loop (cdr pat)
                (+ n (car pat))
                (+ i 1))
          scale))))
(define-syntax mos
  (syntax-rules ()
    ((mos (sizes ...)
          (defaults ...)
          pattern ...)
     (letrec
         ((func
           (case-lambda
             ((equave)
              (func equave defaults ...))
             ((equave sizes ...)
              (edi-scale-pattern equave pattern ...)))))
       func))))
(define mos:citric
  (mos (l s) (2 1)
       l l s l l s))
(define mos:diatonic
  (mos (l s) (2 1)
       l l s l l l s))
(define mos:monowood
  (mos (l s) (2 1)
       l s))
(define mos:antrial
  (mos (l s) (2 1)
       l s s))
(define mos:trial
  (mos (l s) (2 1)
       l l s))
(define mos:antetric
  (mos (l s) (2 1)
       l s s s))
(define mos:biwood
  (mos (l s) (2 1)
       l s l s))
(define mos:tetric
  (mos (l s) (2 1)
       l l l s))
(define mos:pedal
  (mos (l s) (2 1)
       l s s s s))
(define mos:pentic
  (mos (l s) (2 1)
       l s l s s))
(define mos:antipentic
  (mos (l s) (2 1)
       l l s l s))
(define mos:manual
  (mos (l s) (2 1)
       l l l l s))
(define mos:lemon
  (mos (l s) (2 1)
       l l s l s l l s l s))
(define mos:oneirotonic
  (mos (l s) (2 1)
       l l s l l s l s))
(define 5-limit-minor '#(1 10/9 6/5 4/3 3/2 8/5 9/5 2/1))
(define 5-limit-spicy '#(1 16/15 5/4 4/3 3/2 8/5 15/8 2/1))
(define 7-limit-thing '#(1 8/7 9/7 4/3 3/2 49/32 49/27 2/1))
(define mos-5L5s-9/4-equivalent-ultrasoft
  (pipe>> '(0 7 16 23 32 39 48 55 64 71 80)
          (map (lambda (n) (expt 9/4 (/ n 80))))
          (list->vector)))
(define spicier-5-limit
  (pipe>> (rank-2-temperament 5/4 2/1 3)
          (merge-scale-offset 3/1)))
(define 11-limit-thing
  (pipe>> (rank-2-temperament 11 3/2 24)
          (vector-map inexact)))

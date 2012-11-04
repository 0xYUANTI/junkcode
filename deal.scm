;;; deal --- compute combination probabilities for card draws
;;;
;;; Based on the `cardprobs' program written by Jeremy C. York
;;; <jeremy@stat.cmu.edu> in September 1993; cleanup, comments,
;;; generalization, and interface improvements by Eric Raymond
;;; <esr@snark.thyrsus.com> in April 1995.  The -g and -w options
;;; were added by esr September 1996.  `deal' rewritten in Scheme
;;; by Jakob Sievers <j@svirfneblin.org> in December 2008.
;;;
;;; Compile with:
;;;
;;;      csc deal.scm
;;;
;;; Example usage:
;;;
;;; What is the probability (in %) of seeing AT LEAST ONE of my five
;;; `Eyes of Chaos' (assuming a 90-card deck) after drawing nine cards
;;; in addition to the initial hand of seven (> <'s inserted to
;;; highlight the relevant part of deal's output)?
;;;
;;; $ ./deal -g 5 90
;;; 7 in initial hand, 5 specials out of 90 total
;;;
;;; D/S |   0   1   2   3   4   5
;;; -----------------------------
;;; 0   | 100  34   5   .   .   .
;;; 1   | 100  38   6   .   .   .
;;; 2   | 100  42   8   1   .   .
;;; 3   | 100  45   9   1   .   .
;;; 4   | 100  49  11   1   .   .
;;; 5   | 100  52  13   2   .   .
;;; 6   | 100  55  15   2   .   .
;;; 7   | 100  58  17   3   .   .
;;; 8   | 100  61  19   3   .   .
;;; 9   | 100 >63< 21   4   .   .
;;; 10  | 100  66  24   4   .   .
;;; 11  | 100  68  26   5   1   .
;;; 12  | 100  70  28   6   1   .
;;; 13  | 100  72  31   7   1   .
;;; 14  | 100  74  33   8   1   .
;;; 15  | 100  76  36   9   1   .
;;; 16  | 100  78  38  10   1   .
;;; 17  | 100  80  40  12   2   .
;;; 18  | 100  81  43  13   2   .
;;; 19  | 100  83  45  14   2   .
;;; 20  | 100  84  47  16   3   .
;;;
;;; What is the probability of seeing EXACTLY ONE `Cock Robin' in my
;;; initial crypt draw if three of my twelve vampires are `Cock
;;; Robins'?
;;;
;;; $ ./deal -b 4 3 12
;;; 4 in initial hand, 3 specials out of 12 total
;;;
;;; D/S |   0   1   2   3
;;; ---------------------
;;; 0   |  25 >51< 22   2
;;; 1   |  16  48  32   5
;;; 2   |   9  41  41   9
;;; 3   |   5  32  48  16
;;; 4   |   2  22  51  25
;;; 5   |   .  12  49  38
;;; 6   |   .   5  41  55
;;; 7   |   .   .  25  75
;;; 8   |   .   .   . 100
;;;

(use srfi-1 test tool format-modular)

(define (main)
  (define (usage)
    (err "usage: deal [-b initial] [-d draw] [-g] specials total"))
  (define (err msg)
    (print msg)
    (exit 1))

  (define-option #\b "")
  (define-option #\d "")
  (define-flag   #\g "")

  (tool-main
   (command-line-arguments)
   (lambda (args)
     (receive (initial draw specials total)
         (condition-case
          (let* ((specials (string->number (car args)))
                 (total    (string->number (cadr args)))
                 (initial  (if (not b)
                               (if (< total 7) total 7)
                               (string->number b)))
                 (draw     (if (not d)
                               (if (< (- total initial) 20)
                                   (- total initial)
                                   20)
                               (string->number d))))
            (assert (= (length args) 2))
            (+ initial draw specials total) ;raise exn
            (values initial draw specials total))
          ((exn) (usage)))

       (cond ((< total specials)
              (err "Can't have more specials than total cards!"))
             ((<= total initial)
              (err "All specials would be in the initial hand!"))
             ((> draw (- total initial))
              (err "The deck can't last that long!")))

       (format #t
               "~A in initial hand, ~A specials out of ~A total~%~%"
               initial specials total)

       (let ((table (compute-table specials total initial draw)))
         (print-table (if g (map exactly->at-least table) table)))))))

;; M, N: fixed
;; k: initial..initial+draw
;; i: 0..M

(define (compute-table M N initial draw)
  (map (compute-row M)
       (map (lambda (k) (P M N k))
            (map (lambda (n) (+ initial n)) (iota (add1 draw))))))

(define ((compute-row M) P)
  (map P (iota (add1 M))))

(define (exactly->at-least row)
  (map (lambda (idx) (apply + (drop row idx)))
       (iota (length row))))

;; In the table output, a dot designates a probability less than .5% that
;; rounds to zero.

(define (print-table table)
  (let* ((table  (table-map inexact->output table))
         (header (format "D/S |~{~4@A~}" (iota (count-columns table))))
         (body   (format "~:{~&~4A|~@{~4@A~}~}" (numbered table))))
    (print header)
    (print (make-string (string-length header) #\-))
    (print body)))

(define (table-map fun table)
  (map (lambda (row) (map fun row)) table))

(define (inexact->output n)
  (let ((n (round (* n 100))))
    (if (= n 0.0)
        #\.
        (inexact->exact n))))

(define (count-columns table)
  (length (car table)))

(define (numbered table)
  (map (lambda (row n) (cons n row))
       table
       (iota (length table))))

;; If I have M special cards in my deck, and N total cards in my deck,
;; and I draw k cards from that deck (*without replacement* -- the
;; fact that you don't put them back into the deck can be important
;; for some values of M and N), and if X counts the number of special
;; cards contained in those k drawn cards...
;;
;; Then X has a hypergeometric distribution, with the following formula
;; for probabilities:
;;
;;                 B(M, i) * B(N-M, k-i)
;;      P(X = i) = ---------------------
;;                       B(N, k)

(define ((P M N k) i)
  (/ (* (B M i) (B (- N M) (- k i)))
     (B N k)))

;; where B(a,b) is read "a choose b", and is the number of ways
;; to choose b objects out of a possible objects.  The formula for it is
;;
;;     B(a,b)  =   a!/(b! (a-b)!)

(define (B a b)
  (cond ((< a 0) (error "undefined"))
        ((< b 0) 0)
        ((= b 0) 1)
        ((= b a) 1)
        ((> b a) 0)
        (else
         (/ (! a)
            (* (! b) (! (- a b)))))))

(define (! n)
  (letrec ((fac
            (lambda (n acc)
              (if (= n 1)
                  acc
                  (fac (- n 1) (* n acc))))))
    (cond ((< n 0) (error "undefined"))
          ((= n 0) 1)
          (else    (fac n 1)))))

(eval-when (eval)
 (test-error (B -1 0))
 (test  0 (B 3 -1))
 (test  1 (B 0 0))
 (test  1 (B 3 3))
 (test  0 (B 2 3))
 (test 10 (B 5 2))

 (test-error (! -1))
 (test 1 (! 0))
 (test 1 (! 1))
 (test 2 (! 2))
 (test 6 (! 3)))

(eval-when (load) (main))

;;; eof

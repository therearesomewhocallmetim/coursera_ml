#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below



;; 1 ;;
(define (seq1 low high stride)
  (if (> low high)
      null
      (cons low (seq1 (+ low stride) high stride))))

(define (string-append-map xs suffix) 
  (map (lambda (root) (string-append root suffix)) xs))



(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (nth xs (remainder n (length xs))))))



(define (nth xs n)
  (if (= 0 n)
      (car xs)
      (nth (cdr xs) (- n 1))))







#lang racket
(require "CircuitSimulator.rkt")
(require rackunit)

(define sim (make-sim))

(define (mux input1 input2 select)
  (wire-or (wire-and input1 select) (wire-and input2 (wire-not select) )))

(define a (make-wire sim))
(define b (make-wire sim))
(define s (make-wire sim))
(define mux1 (mux a b s))

(wire-set! a #f)
(wire-set! b #f)
(wire-set! s #f)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value b))

(wire-set! a #f)
(wire-set! b #t)
(wire-set! s #f)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value b))

(wire-set! a #t)
(wire-set! b #f)
(wire-set! s #f)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value b))

(wire-set! a #t)
(wire-set! b #t)
(wire-set! s #f)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value b))



(wire-set! a #f)
(wire-set! b #f)
(wire-set! s #t)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value a))

(wire-set! a #f)
(wire-set! b #t)
(wire-set! s #t)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value a))

(wire-set! a #t)
(wire-set! b #f)
(wire-set! s #t)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value a))

(wire-set! a #t)
(wire-set! b #t)
(wire-set! s #t)
(sim-wait! sim 20)
(check-equal? (wire-value mux1) (wire-value a))

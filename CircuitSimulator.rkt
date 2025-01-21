#lang racket
;Arti Brzozowski

(require data/heap)
(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))


;-----------------------------SYMULATION-----------------------------
(struct sim ([time #:mutable] [que #:mutable]))

;Create simulation
(define (make-sim)
  (sim 0 (make-heap (lambda (p1 p2) (< (car p1)
                                       (car p2))))))

;Action call
(define (call-actions! sm)
  (let ([que (sim-que sm)])
    (cond
      [(= 0 (heap-count que)) (void)]
      [(<= (car (heap-min que)) (sim-time sm)) (begin
                                                 ((cdr (heap-min que)))
                                                 (heap-remove-min! que)
                                                 (call-actions! sm))]
      [else (void)])))

;Set next simulation time
(define (set-next-time! sm t)
  (let ([que (sim-que sm)])
    (if (> (heap-count que) 0)
        (if (<= (car (heap-min que)) t)
            (set-sim-time! sm (car (heap-min que)))
            (set-sim-time! sm t))
        (set-sim-time! sm t))))

;Time shift and triggering actions
(define (sim-wait! sm time)
  (define (sim-wait-aux! sm final-time)
    (if (eq? (sim-time sm) final-time)
        (void)
        (begin
          (set-next-time! sm final-time)
          (call-actions! sm)
          (sim-wait-aux! sm final-time))))
  (sim-wait-aux! sm (+ (sim-time sm) time)))

;Add new action
(define (sim-add-action! sm time action)
  (heap-add! (sim-que sm) (cons time action)))


;-----------------------------WIRES-----------------------------
(struct wire (sim [value #:mutable] [actions #:mutable]))

;Create new wire
(define (make-wire sm)
  (wire sm #f '()))

;Add action, which will be triggered when wire value changes
(define (wire-on-change! w action)
  (begin
    (action)
    (set-wire-actions! w (cons action (wire-actions w)))))

;Auxiliary procedure to trigger a wire action
(define (call-wire-actions sm lst)
  (if (empty? lst)
      (void)
      (begin
        ((car lst))
        (call-wire-actions sm (cdr lst)))))

#| Changing the value of the wire and invoking the actions assigned to it
When the value of the wire changes, we call its actions
which will add an action to the simulation for each gate |#
(define (wire-set! w v)
  (if (eq? (wire-value w) v)
      (void)
      (begin
        (set-wire-value! w v)
        (call-wire-actions (wire-sim w) (wire-actions w)))))


;-----------------------------GATES-----------------------------
#| The gate does not need to add an action to the simulation,
it is enough to define what should happen when the wire value changes |#

;Procedure for creating two-input gates
(define (gate-2ins out in1 in2 action time)
  (if (and (equal? (wire-sim in1)
                   (wire-sim out))
           (equal? (wire-sim in1)
                   (wire-sim in2)))
      (begin
        (wire-on-change! in1 (lambda () (sim-add-action! (wire-sim in1)
                                                         (+ time (sim-time (wire-sim in1)))
                                                         action)))
        (wire-on-change! in2 (lambda () (sim-add-action! (wire-sim in2)
                                                         (+ time (sim-time (wire-sim in2)))
                                                         action))))
      (error "Wires from different simulations")))

;Procedures for not, and, nand, or, nor, xor gates
(define (gate-not out in)
  (let ([action (lambda () (wire-set! out (not (wire-value in))))])
    (if (equal? (wire-sim in) (wire-sim out))
        (wire-on-change! in (lambda () (sim-add-action! (wire-sim in)
                                                        (+ 1 (sim-time (wire-sim in)))
                                                        action)))
        (error "Wires from different simulations"))))

(define (gate-and out in1 in2)
  (gate-2ins out
             in1
             in2
             (lambda () (wire-set! out (and (wire-value in1)
                                            (wire-value in2))))
             1))

(define (gate-nand out in1 in2)
  (gate-2ins out
             in1
             in2
             (lambda () (wire-set! out (not (and (wire-value in1)
                                                 (wire-value in2)))))
             1))

(define (gate-or out in1 in2)
  (gate-2ins out
             in1
             in2
             (lambda () (wire-set! out (or (wire-value in1)
                                           (wire-value in2))))
             1))

(define (gate-nor out in1 in2)
  (gate-2ins out
             in1
             in2
             (lambda () (wire-set! out (not (or (wire-value in1)
                                                (wire-value in2)))))
             1))

(define (gate-xor out in1 in2)
  (gate-2ins out
             in1
             in2
             (lambda () (wire-set! out (and (not (and (wire-value in1)
                                                      (wire-value in2)))
                                            (or (wire-value in1)
                                                (wire-value in2)))))
             2))


;-----------------------------WIRES WITH GATES-----------------------------
;Procedure for creating gates (with two inputs) connected to a wire
(define (wire-2ins in1 in2 gate)
  (if (equal? (wire-sim in1) (wire-sim in2))
      (let ([out (make-wire (wire-sim in1))])
        (begin
          (gate out in1 in2)
          out))
      (error "Wires from different simulations")))

(define (wire-not in)
  (let ([out (make-wire (wire-sim in))])
    (begin
      (gate-not out in)
      out)))

(define (wire-and in1 in2)
  (wire-2ins in1 in2 gate-and))

(define (wire-nand in1 in2)
  (wire-2ins in1 in2 gate-nand))

(define (wire-or in1 in2)
  (wire-2ins in1 in2 gate-or))

(define (wire-nor in1 in2)
  (wire-2ins in1 in2 gate-nor))

(define (wire-xor in1 in2)
  (wire-2ins in1 in2 gate-xor))
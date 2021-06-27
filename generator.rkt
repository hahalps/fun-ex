#lang plait


(define (make-number n)
  (make-generator (lambda(yield)
                    (lambda(vd)
                      (local [(define (number n)
                                (begin
                                  (yield n)
                                  (number (+ n 1))))]
                        (number n))))))

(define (make-generator proc)
  (local [(define yield (lambda(v) v))
          (define go (proc (lambda(v) (yield v))))]
    (lambda()
      (let/cc escape
          (begin
            (set! yield
                  (lambda(v)
                    (let/cc k
                      (begin
                        (set! go k)
                        (escape v)))))
            (go (void)))))))

;; test
(define g (make-number 0))
(g)
(g)
(g)
(g)


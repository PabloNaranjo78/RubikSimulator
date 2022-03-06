#lang scheme
;Largo de una lista
(define (largo lista)
  (cond ((null? lista)
         0)
        (else
         (+ 1 (largo (cdr lista))))))

;Extraer el n-esimo elemento de una lista
(define (elemento index lista)
  (cond ((null? lista)
         #f)
        ((equal? index 1)
         (car lista))
         (else
          (elemento (- index 1)(cdr lista)))))

;Eliminar un elemento de una lista
(define (eliminar ele lista)
  (cond ((null? lista)
         '())
        ((equal? ele (car lista))
         (eliminar ele (cdr lista)))
        (else
         (cons (car lista)
               (eliminar ele (cdr lista))))))

;Extraer el ultimo elemento de una lista
(define (ultimo lista)
  (elemento (largo lista) lista))

;Sustituir un elemento de una lista
(define (sus ele lista newL)
  (sus-aux ele lista newL 1))

(define (sus-aux ele lista newL cont)
  (cond ((null? lista)
         '())
        ((equal? ele cont)
         (cons newL (sus-aux ele (cdr lista) newL (+ cont 1))))
        (else
         (cons (car lista)(sus-aux ele (cdr lista) newL (+ cont 1))))))

;Invertir una lista
(define (invertir lista)
  (cond ((null? lista)
         '())
        (else
         (append (invertir (cdr lista))
                 (list (car lista))))))
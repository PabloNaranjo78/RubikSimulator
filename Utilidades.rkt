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

;Extraer el ultimo elemento de una lista
(define (ultimo lista)
  (elemento (largo lista) lista))

;Eliminar un elemento de una lista
(define (eliminar ele lista)
  (cond ((null? lista)
         '())
        ((equal? ele (car lista))
         (eliminar ele (cdr lista)))
        (else
         (cons (car lista)
               (eliminar ele (cdr lista))))))

;Sustituir un elemento de una lista
(define (sus pos lista newE) ;pos = posicion
  (sus-aux pos lista newE 1))

(define (sus-aux pos lista newE cont)
  (cond ((null? lista)
         '())
        ((equal? pos cont)
         (cons newE (sus-aux pos (cdr lista) newE (+ cont 1))))
        (else
         (cons (car lista)(sus-aux pos (cdr lista) newE (+ cont 1))))))

;Invertir una lista
(define (invertir lista)
  (cond ((null? lista)
         '())
        (else
         (append (invertir (cdr lista))
                 (list (car lista))))))

;Rotar una cara del cubo
;(define (rotarF matriz cara) ;cara = cual de todas las caras
;  (rotarF-aux matriz cara dir 1))

;(define (rotarF-aux matriz cara dir cont)
;  ())

;Invertir la lista y luego aplicar car car car


;Sustituir un elemento de una lista
;(define (rotar-centro cubo pos dir)
;  (fila-aux cubo pos dir (ultimo (elemento 1 cubo))
;            (elemento pos (elemento 1 cubo)) 1));num = numero de fila, ult = ultimo, pri = primero
;
;(define (rotar-centro-aux cubo pos dir ult pri cont)
;  (cond ((= cont 5)
;         (display "termina el ciclo"))
;        ((equal? cont cont)
;         (cons ult (rotar-centro-aux pos (cdr lista) newL (+ cont 1))))
;        (else
;        (cons (car(car lista))(rotar-centro-aux pos (cdr lista) newL (+ cont 1))))))
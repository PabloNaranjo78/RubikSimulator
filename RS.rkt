#lang scheme

(require "Utilidades.rkt")

;(define (nth-column M n)
  ;(map (lambda (row) (list-ref row n)) M))

;(define (nth-row M n)
  ;(map (lambda (column) (list-ref column n)) M))

;(define M '((1 2 3)
            ;(2 3 4)
           ; (6 7 9)))

;(nth-column M 1)

;(nth-row M 0)

;(nth-column M 0)

;(nth-column M 2)


(define (RS Tam Matriz Movs)
  (cond
    ;Condicion para verificar correcta cantidad de caras (6)
    ((and (equal? (largo Matriz) 6) (lenFil Tam Matriz 1 1)) (display "ciclo terminado")) ;(lenCol Tam Matriz)) "Funcion de Movs"))
          (else
           (display "error"))))

     (define (lenFil Tam Matriz cont1 cont2)
       (cond
         ((> cont2 6) #t)
         (else (lenFil-aux Tam Matriz cont1 cont2))))
    

     (define (lenFil-aux Tam Matriz cont1 cont2)
       (cond
         ((equal? Tam (largo '(3 3))) (lenFil-aux Tam Matriz (+ 1 cont1) (+ 1 cont2 )))
                                                                    (else 
                                                                     (#f))))
   

;(largo (elemento cont1 (elemento 1 Matriz)))        
;(define (lenCol Tam Matriz) 
;(cond
 ; [(empty? (elemento2 0 Matriz)) 0]
 ; [else (+ (elemento2 0 Matriz) ( lenCol(rest '(elemento2 0 Matriz))))]))
                                                                                       
;(define (elemento index lista)
 ; (cond ((null? lista)
     ;    #f)
     ;   ((equal? index 1)
      ;   (car lista))
      ;   (else
       ;   (elemento (- index 1)(cdr lista)))))

;(define (elemento2 index elemento)
  ;(cond ((null? elemento)
      ;   #f)
       ; ((equal? index 1)
      ;   (car elemento))
      ;   (else
       ;   (elemento2 (- index 1)(cdr elemento)))))

;(define (movimiento last Matriz)
  ;(cond
    ;((if length last Matriz) 0)

(RS  3 '( ( (A A A)
            (A A A)
            (A A A) )

          ( (B B B)
            (B B B)
            (B B B) )

          ( (V V V)
            (V V V)
            (V V V) ) 

          ( (Y Y Y) 
            (Y Y Y)
            (Y Y Y) ) 

          ( (R R R) 
            (R R R)
            (R R R) )

          ( (N N N)
            (N N N) 
            (N N N) ) ) '(F1D C2A F3I))

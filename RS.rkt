#lang scheme

(require "Utilidades.rkt")

(define (RS Tam Matriz Movs)
  (cond
    ;Condicion para verificar correcta cantidad de caras (6)
    ((and (equal? (largo Matriz) 6)  (lenFilCol Tam Matriz 1 1))  (display "ciclo terminado"))  ;"Funcion de Movs"))
    (else
     (display "error"))))

(define (lenFilCol Tam Matriz cont cont2)
  (cond
    ((> (largo(elemento cont Matriz)) Tam) #f)
    ((= cont2 6) #t)
    ((> cont Tam) (lenFilCol-aux Tam Matriz 1 (+ 1 cont2)))
    (else (lenFilCol-aux Tam Matriz cont cont2))))
    

(define (lenFilCol-aux Tam Matriz cont cont2)
  (cond
    ((equal? (largo(elemento cont (elemento cont2 Matriz))) Tam) (lenFilCol Tam Matriz (+ 1 cont ) cont2))
  (else #f)))

   
;(largo (elemento cont1 (elemento 1 Matriz)))        
;(define (lenCol Tam Matriz cont) 
 ; (cond
 ;   [(empty? (elemento2 0 Matriz)) #t]
  ;  [else (+ (elemento2 0 Matriz) ( lenCol(rest '(elemento2 0 Matriz))))]))
                                                                                       






(RS  3 '( ( (A A A)
            (A A A)
            (A A A))

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

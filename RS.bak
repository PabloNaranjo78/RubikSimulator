#lang scheme

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

            
(define Matriz (list 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) '(F1D C2A F3I)))

;(define (RS Matriz)
  ;(cond
    ;((if (first Matriz 2)
        ;      [(length second Matriz 2) "funcion para verificar tamano del columna" (error "tamano de fila mala")])
     ; (if (first Matriz 3)
      ;        [(length second Matriz 3) "funcion para verificar tamano del columna" (error "tamano de fila mala")])
       ;  ((if (first Matriz 4)
      ;   cond(
     ;         [(length second Matriz 4) "funcion para verificar tamano del columna" (error "tamano de fila mala")]))
     ; (if (first Matriz 5)
      ;   cond(
       ;       [(length second Matriz 5) "funcion para verificar tamano del columna" (error "tamano de fila mala")]))
       ;  ((else (first Matriz 6)
       ;  cond(
           ;   [(length second Matriz 6) "funcion para verificar tamano del columna" (error "tamano de fila mala")])))))))
        
(define (lenCol Matriz)
(cond
  [(empty? (elemento2 0 Matriz)) 0]
  [else (+ (elemento2 0 Matriz) ( lenCol(rest '(elemento2 0 Matriz))))]))
                                                                                       
(define (elemento index lista)
  (cond ((null? lista)
         #f)
        ((equal? index 1)
         (car lista))
         (else
          (elemento (- index 1)(cdr lista)))))

(define (elemento2 index elemento)
  (cond ((null? elemento)
         #f)
        ((equal? index 1)
         (car elemento))
         (else
          (elemento2 (- index 1)(cdr elemento)))))

(lenCol Matriz)

;(define (movimiento last Matriz)
  ;(cond
    ;((if length last Matriz) 0)


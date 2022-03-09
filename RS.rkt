#lang scheme

(require "Utilidades.rkt")
(require "GUI.rkt")

;RS: Funcion que llama a la parte grafica despues de hacer la verificaciones de las entradas
;Entradas: Tamano del cubo / El cubo en forma de matriz / lista de movimientos
;Salidas: Funcion de iniciar el gui
(define (RS Tam Matriz Movs)
  (cond
    ;Condicion para verificar correcta cantidad de caras (6)
    ((and (equal? (largo Matriz) 6)  (lenFilCol Tam Matriz 1 1) (verificarMovs Tam Movs)) (iniciar Tam Matriz Movs))
    (else
     (display "error"))))

;lenFilCol: Funcion que verifica lo largo de las filas, las columnas y si el cubo tiene 6 caras
;Entradas:  Tamano del cubo / El cubo en forma de matriz / lista de movimientos / contador / contador 2
;Salidas: #true or #false
(define (lenFilCol Tam Matriz cont cont2)
  (cond
    ((> (largo(elemento cont Matriz)) Tam) #f)
    ((= cont2 6) #t)
    ((> cont Tam) (lenFilCol-aux Tam Matriz 1 (+ 1 cont2)))
    (else (lenFilCol-aux Tam Matriz cont cont2))))
    

;lenFilCol-aux: Funcion auxiliar que hace recursion a lenFilCol
;Entradas: Tamano del cubo / El cubo en forma de matriz / lista de movimientos / contador / contador 2
;Salidas: False or funcion lenFilCol
(define (lenFilCol-aux Tam Matriz cont cont2)
  (cond
    ((equal? (largo(elemento cont (elemento cont2 Matriz))) Tam) (lenFilCol Tam Matriz (+ 1 cont ) cont2))
    (else #f)))

;verificarMovs: Funcion que verifica que los movimientos tengan formato de (ej:(F1D))
;Entradas: Tamano del cubo / lista de movimientos
;Salidas: #t or verificarMovs-aux
(define (verificarMovs Tam Movs)
  (cond
    ((null? Movs)#t)
    (else
     (verificarMovs-aux Tam Movs))))

;verificarMovs-aux: Funcion auxiliar recursiva de verificarMovs
;Entradas: Tamano del cubo / lista de movimientos
;Salidas: #f or recursion de verificarMovs

(define (verificarMovs-aux Tam Movs)
  (cond
    ((and
      (<= (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) Tam)
      (>=  (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) 1)
      (= (string-length (symbol->string (car Movs))) 3))
     (verificarMovs Tam (cdr Movs)))
    (else #f)))

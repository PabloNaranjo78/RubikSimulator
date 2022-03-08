#lang scheme
(provide elemento)
(provide largo)

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

(define (aplastar lista)
  (cond ((null? lista)
         '())
        ((list? lista)
         (list (aplastar (car lista)) (aplastar (cdr lista)) ))
        (else
         (lista)))
  )

(define (concatenar_listas lista1 lista2)
  (if (and (null? lista1) (null? lista2))
      '()
      (if (not (null? lista1))
          (cons (car lista1) (concatenar_listas (cdr lista1) lista2))
          (if (not (null? lista2))
              (cons (car lista2) (concatenar_listas lista1 (cdr lista2)))
              '()
              )
          )
      )
  )


(define (aplanar lista)
  (if (null? lista)
      '()
      (if (list? (car lista))
          (concatenar_listas (aplanar (car lista)) (aplanar (cdr lista)))
          (concatenar_listas (list (car lista)) (aplanar (cdr lista)))
          )
      )
  )

;Validacion fila/columna
(define (fil-col? X Cubo Movs) ;fil-col = fila o columna
  (cond ((or(< X 2)(> X 6))
         (display "El numero ingresado esta fuera del rango"))
        ((or (not(list? Cubo))(null? Cubo))
         (display "El parametro Cubo, no es una lista o esta vacia"))
        ((or (not (list? Movs))(null? Movs))
         (display "El parametro Movs, no es una lista o esta vacia"))
        (else
         (fila-col?-aux X Cubo Movs))))

;La funcion contiene verifica si es fila o columna y su direccion
(define (contiene ele comp1 comp2)
  (and (= (largo(string->list(symbol->string ele))) 3) (equal? comp1 (string (car(string->list(symbol->string ele)))))
              (equal? comp2 (string (car(cddr(string->list(symbol->string ele))))))))

;Separacion de las intrucciones 
(define (fila-col?-aux X Cubo Movs)
  (cond ((contiene (car Movs) "F" "D")
         (display "SI"))
        ((contiene (car Movs) "F" "I")
         (display "SI"))
        ((contiene (car Movs) "C" "A")
         (display "CC"))
        ((contiene (car Movs) "C" "B")
         (display "DD"))
        (else
         (display "Instruccion incorrecta"))))

;Validacion del tamaño del cubo
(define (tamano-cubo X Cubo Movs dir) ;fila-der = fila derecha
  (cond ((equal? X 2)
         (display "Funcion maximos"))
        (else
         (fila-der-aux X Cubo Movs dir))))

;Fila central(hacia la derecha) para cualquier cubo 3x3 o mayor
(define (fila-der-aux X Cubo Movs dir)
  (cond ((and (> 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))))
                    (< X (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))))))
        (rotar-centro X Cubo Movs dir)
  (else
   (extremos X Cubo Movs dir))))

(define (extremos X Cubo Movs dir)
  (cond ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "D"))
         ("Funcion para rotar los bordes"))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "I"))
         ("Funcion para rotar los bordes 3 veces derecha"))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "D"))
         ("Funcion para rotar los bordes"))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "I"))
         ("Funcion para rotar los bordes 3 veces derecha"))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "A"))
         ("Funcion para rotar bordes arriba"))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "B"))
         ("Funcion para rotar bordes arriba 3 veces"))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "A"))
         ("Funcion para rotar bordes arriba"))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "B"))
         ("Funcion para rotar bordes arriba 3 veces"))
        (else
         (display "HUBO UN ERROR"))))

;Sustituir un elemento de una lista
;(define (rotar-centro X Cubo Movs dir)
;  (fila-aux cubo pos dir (ultimo (elemento 1 cubo))
;            (elemento pos (elemento 1 cubo)) 1));num = numero de fila, ult = ultimo, pri = primero

;(define (rotar-centro-aux cubo pos dir ult pri cont)
;  (cond ((= cont 5)
;         (display "termina el ciclo"))
;        ((equal? cont cont)
;         (cons ult (rotar-centro-aux pos (cdr lista) newL (+ cont 1))))
;        (else
;        (cons (car(car lista))(rotar-centro-aux pos (cdr lista) newL (+ cont 1))))))

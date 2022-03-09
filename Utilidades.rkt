#lang scheme
(provide elemento)
(provide largo)
(provide fil-col?)

(define matriz '( ( (A A A)
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
            (N N N) ) ))

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
         (tamano-cubo X Cubo Movs "D"))
        ((contiene (car Movs) "F" "I")
         (tamano-cubo X Cubo Movs "I"))
        ((contiene (car Movs) "C" "A")
         (tamano-cubo X Cubo Movs "A"))
        ((contiene (car Movs) "C" "B")
         (tamano-cubo X Cubo Movs "B"))
        (else
         (display "Instruccion incorrecta"))))

;Validacion del tamaño del cubo
(define (tamano-cubo X Cubo Movs dir) ;fila-der = fila derecha
  (cond ((equal? X 2)
         (rotarIzquierdaMatriz matriz))
        (else
         (verificar-centro X Cubo Movs dir))))

;Fila central(hacia la derecha) para cualquier cubo 3x3 o mayor
(define (verificar-centro X Cubo Movs dir)
  (cond ((and (< 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))))
                    (> X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))))
         (actualizar-fila Cubo
                         (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                         1
                         Movs
                         (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo))))      
  (else
   (esquinas X Cubo Movs dir))))

;
(define (actualizar-fila Cubo fila cont Movs primero)
  (cond ((= cont 4)
         (cambiar-fila cont fila Cubo primero))
        (else
         (println (cambiar-fila cont fila Cubo primero))
         (actualizar-fila-aux Cubo fila cont Movs primero))))

(define (actualizar-fila-aux Cubo fila cont Movs primero)
          (actualizar-fila (cambiar-fila cont
                                         fila
                                         Cubo
                                         (elemento fila (elemento (+ cont 1) Cubo)))
                           fila (+ cont 1) Movs primero))

(define (cambiar-fila cara fila Cubo newfila)
  (sus cara Cubo (sus fila (elemento cara Cubo) newfila))) 

;
(define (esquinas X Cubo Movs dir)
  (cond ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "D"))
         (rotarDerechaMatriz matriz))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "I"))
         (rotarIzquierdaMatriz matriz))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "D"))
         (rotarDerechaMatriz matriz))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "I"))
         (rotarIzquierdaMatriz matriz))
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

(define (rotarIzquierdaMatriz matriz)
  (rotarDerechaMatriz(rotarDerechaMatriz(rotarDerechaMatriz matriz))))

(define (rotarDerechaMatriz matriz)
  (rotarDerechaMatriz-aux matriz 1 (largo matriz)))

(define (rotarDerechaMatriz-aux matriz cont tam)
  (cond ((> cont tam) '())
        (else
         (cons (subElementoN matriz cont) (rotarDerechaMatriz-aux matriz (+ 1 cont) tam)))))

(define (subElementoN matriz N)
  (subElementoN-aux (invertir matriz) N 1 (largo matriz)))

(define (subElementoN-aux matrix N cont tam)
  (cond  ((> cont tam) '())
         (else
           (cons (elemento N (elemento cont matrix)) (subElementoN-aux matrix N (+ 1 cont) tam) ))))


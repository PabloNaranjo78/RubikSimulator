#lang scheme
(provide elemento)
(provide largo)
(provide update)
(provide rotarIzquierdaMatriz)
(provide rotarDerechaMatriz)


;Descripcion: Regresa el largo de una lista
;Entradas: lista
;Salidas: numero entero
(define (largo lista)
  (cond ((null? lista)
         0)
        (else
         (+ 1 (largo (cdr lista))))))

;Descripcion: Extrae el n-esimo elemento de una lista
;Entradas: index(n-esimo elemento que se desea extraer), lista
;Salidas: n-esima elemento
(define (elemento index lista)
  (cond ((null? lista)
         #f)
        ((equal? index 1)
         (car lista))
        (else
         (elemento (- index 1)(cdr lista)))))

;Descripcion: Extrae el ultimo elemento de una lista
;Entradas: lista
;Salidas: ultimo elemento de una lista
(define (ultimo lista)
  (elemento (largo lista) lista))

;Descripcion: Elimina un elemento de una lista
;Entradas: ele(elemento que se desea eliminar dentro de la lista), lista
;Salidas: regresa la lista con el elemento eliminado
(define (eliminar ele lista)
  (cond ((null? lista)
         '())
        ((equal? ele (car lista))
         (eliminar ele (cdr lista)))
        (else
         (cons (car lista)
               (eliminar ele (cdr lista))))))

;Descripcion: Sustituye un elemento de una lista
;Entradas: pos(posicion dentro de la lista que se desea cambiar),lista, newE(nuevo elemento que remplaza el elemento en la posicion indicada)
;Salidas: regresa la lista completa con el elemento cambiado
(define (sus pos lista newE) 
  (sus-aux pos lista newE 1))

(define (sus-aux pos lista newE cont)
  (cond ((null? lista)
         '())
        ((equal? pos cont)
         (cons newE (sus-aux pos (cdr lista) newE (+ cont 1))))
        (else
         (cons (car lista)(sus-aux pos (cdr lista) newE (+ cont 1))))))

;Descripcion: Invierte una lista
;Entradas: lista
;Salidas: regresa la lista con los elementos invertidos
(define (invertir lista)
  (cond ((null? lista)
         '())
        (else
         (append (invertir (cdr lista))
                 (list (car lista))))))

;Descripcion: Revisa primero que el tamaño del cubo este dentro del rango, segundo que Cubo sea una lista y que no este vacia,
;tercero que Movs sea una lista y que no este vacia
;Entradas: X(tamaño del cubo), Cubo(matriz con los colores del cubo), Movs(lista de movimientos)
;Salidas: Regresa un error en caso de que alguna de las condiciones no se cumpla, en el caso exitoso se envia a la funcion fila-col?
(define (update X Cubo Movs) 
  (cond ((or(< X 2)(> X 6))
         (display "El numero ingresado esta fuera del rango"))
        ((or (not(list? Cubo))(null? Cubo))
         (display "El parametro Cubo, no es una lista o esta vacia"))
        ((or (not (list? Movs))(null? Movs))
         (display "El parametro Movs, no es una lista o esta vacia"))
        (else
         (fila-col? X Cubo Movs))))

;Descripcion: Lee la primera intruccion en la lista de movimientos y la separa por fila o columna y direccion
;Entradas: X(tamaño del cubo), Cubo(matriz con los colores del cubo), Movs(lista de movimientos)
;Salidas: Se llama a la funcion tamano-cubo, los nuevos parametros son fil-col(fila o columna) y dir(direccion)
(define (fila-col? X Cubo Movs)
  (cond ((contiene (car Movs) "F" "D")
         (tamano-cubo X Cubo Movs "F" "D"))
        ((contiene (car Movs) "F" "I")
         (tamano-cubo X Cubo Movs "F" "I"))
        ((contiene (car Movs) "C" "A")
         (tamano-cubo X Cubo Movs "C" "A"))
        ((contiene (car Movs) "C" "B")
         (tamano-cubo X Cubo Movs "C" "B"))
        (else 
         (display "Instruccion incorrecta"))))

;Descripcion: La funcion verifica si es fila o columna y su direccion
;Entradas: ele(primera instruccion de la lista de movimientos), comp1(fila o columa), comp2(direccion)
;Salidas: #t o #f
(define (contiene ele comp1 comp2)
  (and (= (largo(string->list(symbol->string ele))) 3) (equal? comp1 (string (car(string->list(symbol->string ele)))))
       (equal? comp2 (string (car(cddr(string->list(symbol->string ele))))))))

;Descripcion: Valida el tamaño del cubo
;Entradas: X(tamaño del cubo), Cubo(matriz con los colores del cubo), fila-col(fila o columa), dir(direccion)
;Salidas: funcion esquinas o fi-col-central?
(define (tamano-cubo X Cubo Movs fil-col dir) 
  (cond ((equal? X 2)
         (esquinas X Cubo Movs dir))
        (else
         (fil-col-central? X Cubo Movs fil-col dir))))

;Descripcion: La funcion determina si la fila/columa que se quiere mover se ubica en el "centro" o es una "esquina"
;Entradas: X(tamaño del cubo), Cubo(matriz con los colores del cubo), fila-col(fila o columa), dir(direccion)
;Salidas: funcion fi-col-central-aux? o esquinas 
(define (fil-col-central? X Cubo Movs fil-col dir)
  (cond ((and (< 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))))
              (> X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))))
         (fil-col-central-aux? X Cubo Movs fil-col dir))      
        (else
         (esquinas X Cubo Movs dir))))

;Descripcion: Distingue entre fila y columna
;Entradas: X(tamaño del cubo), Cubo(matriz con los colores del cubo), fila-col(fila o columa), dir(direccion)
;Salidas: funcion actualizar-fila o actualizar-columna 
(define (fil-col-central-aux? X Cubo Movs fil-col dir)
(cond ((or(equal? dir "I")(equal? dir "A"))
  (cond ((equal? fil-col "F") 
         (actualizar-fila Cubo
                          (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                          1
                          Movs
                          (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) 1 dir))
        ((equal? fil-col "C")
         (actualizar-columna Cubo
                          (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                          1
                          Movs
                          (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) 1 dir))))
  (else
   (cond ((equal? fil-col "F")
          (rotarDer Cubo (obtenerNumero Movs) Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) dir))
         ((equal? fil-col "C")
          (revertirCubo (rotarDer (rotarCubo(col-fin Cubo  (elemento 4 Cubo) 4)) (obtenerNumero Movs) Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) dir)))))))

;Descripcion: Realiza una rotacion hacia la derecha del cubo
;Entradas: Cubo(matriz con los colores del cubo), fila(fila o columa), dir(direccion)
;Salidas: funcion actualizar-fila o actualizar-columna 
(define (rotarDer Cubo fila Movs primero dir)
  (actualizar-fila (actualizar-fila (actualizar-fila Cubo fila 1 Movs
                                                     (elemento
                                                      (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                                                      (elemento 1 Cubo))1 dir)
                                    fila 1 Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 (actualizar-fila Cubo fila 1 Movs
                                                                                                                                                                    (elemento
                                                                                                                                                                     (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                                                                                                                                                                     (elemento 1 Cubo))1 dir))) 1 dir)
                   fila 1 Movs (elemento
                                (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                                (elemento 1 (actualizar-fila (actualizar-fila Cubo fila 1 Movs
                                                     (elemento
                                                      (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                                                      (elemento 1 Cubo))1 dir)
                                    fila 1 Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 (actualizar-fila Cubo fila 1 Movs
                                                                                                                                                                    (elemento
                                                                                                                                                                     (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))
                                                                                                                                                                     (elemento 1 Cubo))1 dir))) 1 dir))) 1 dir))


;Descripcion: La funcion sirve para repetir el ciclo 
;Entradas: Cubo(matriz con los colores del cubo), fila, cont(contador para la cantidad caras que deben cambiar),
;Salidas: funcion cambiar-fila
(define (actualizar-fila Cubo fila cont Movs primero cont2 dir)
  (cond ((or (equal? dir "I") (equal? dir "A"))
         (cond ((= cont 4)                
                (cambiar-fila cont fila Cubo primero))
               (else               
                (actualizar-fila-aux Cubo fila cont Movs primero cont2 dir))))
        ((or (equal? dir "D") (equal? dir "B"))
         (cond ((= cont2 3)
                (cond ((= cont 4)
                      (cambiar-fila cont fila Cubo primero))
                (else
                 (actualizar-fila-aux Cubo fila cont Movs primero cont2 dir)))) 
               (else
                (actualizar-fila-aux Cubo fila cont Movs primero (+ 1 cont2) dir))))
        (else
         (display "Hubo un error"))))

(define (actualizar-fila-aux Cubo fila cont Movs primero cont2 dir)
  (actualizar-fila (cambiar-fila cont
                                 fila
                                 Cubo
                                 (elemento fila (elemento (+ cont 1) Cubo)))
                   fila (+ cont 1) Movs primero cont2 dir))

;Descripcion: Se encarga de sustiuir la fila anterior por la nueva
;Entradas: cara(cara del cubo que se desea cambiar),Cubo(matriz con los colores del cubo), fila, newfila(la fila va a remplzar la anterior)
;Salidas: funcion cambiar-fila
(define (cambiar-fila cara fila Cubo newfila)
  (sus cara Cubo (sus fila (elemento cara Cubo) newfila)))

;Descripcion: retorna la ultima columna
;Entradas: Cubo(matriz con los colores del cubo), cara(cara del cubo que se desea cambiar), pos(posicion donde se desea cambiar el dato)
;Salidas: retorna la ultima columna
(define (col-fin Cubo Cara pos)
                 (sus pos Cubo Cara))

;Descripcion: La funcion verifica la "esquina" y la direccion que se desea cambiar dentro del cubo
;Entradas: X(tamaño del cubo), Cubo(matriz con los colores del cubo), Movs(lista de movimientos), dir(direccion)
;Salidas: funcion actualizar-fila
(define (esquinas X Cubo Movs dir)
  (cond ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "I"))
         (actualizar-fila (col-fin Cubo (rotarDerechaMatriz (elemento 5 Cubo)) 5) (obtenerNumero Movs) 1 Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) 1 dir))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "D"))
         (rotarDer (col-fin Cubo (rotarIzquierdaMatriz (elemento 5 Cubo)) 5) (obtenerNumero Movs) Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) dir))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "D"))
         (rotarDer (col-fin Cubo (rotarDerechaMatriz (elemento 6 Cubo)) 6) (obtenerNumero Movs) Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) dir))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "I"))
         (actualizar-fila (col-fin Cubo (rotarIzquierdaMatriz (elemento 6 Cubo)) 6) (obtenerNumero Movs) 1 Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) 1 dir))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "A"))
         (actualizar-columna (col-fin Cubo (rotarDerechaMatriz (elemento 2 Cubo)) 2) (obtenerNumero Movs) 1 Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) 1 dir))
        ((and (equal? 1 (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "B"))
         (revertirCubo (rotarDer (rotarCubo(col-fin Cubo (rotarIzquierdaMatriz (elemento 2 Cubo)) 2)) (obtenerNumero Movs) Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) dir)))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "A"))
         (actualizar-columna (col-fin Cubo (rotarDerechaMatriz (elemento 4 Cubo)) 4) (obtenerNumero Movs) 1 Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) 1 dir))
        ((and (equal? X (string->number(string (car(cdr(string->list(symbol->string (car Movs)))))))) (equal? dir "B"))
         (revertirCubo (rotarDer (rotarCubo(col-fin Cubo (rotarIzquierdaMatriz (elemento 4 Cubo)) 4)) (obtenerNumero Movs) Movs (elemento (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))) (elemento 1 Cubo)) dir)))
        (else
         (display "HUBO UN ERROR"))))

;Descripcion: realiza una rotacion hacia la izquierda de una cara
;Entradas: matriz(matriz con los colores del cubo)
;Salidas: funcion rotarDerechaMatriz 3 veces(rotar 3 veces derecha es lo mismo que 1 rotacion a la izquierda)
(define (rotarIzquierdaMatriz matriz)
  (rotarDerechaMatriz(rotarDerechaMatriz(rotarDerechaMatriz matriz))))

;Descripcion: realiza una rotacion hacia la derecha de una cara
;Entradas: matriz(matriz con los colores del cubo)
;Salidas: funcion rotarDerechaMatriz
(define (rotarDerechaMatriz matriz)
  (rotarDerechaMatriz-aux matriz 1 (largo matriz)))

(define (rotarDerechaMatriz-aux matriz cont tam)
  (cond ((> cont tam) '())
        (else
         (cons (subElementoN matriz cont) (rotarDerechaMatriz-aux matriz (+ 1 cont) tam)))))

;Descripcion: Devuelve un sub-elemento de una matriz
;Entradas: matriz(matriz con los colores del cubo), N(n-esimo elemento)
;Salidas: funcion rotarDerechaMatriz
(define (subElementoN matriz N)
  (subElementoN-aux (invertir matriz) N 1 (largo matriz)))

(define (subElementoN-aux matrix N cont tam)
  (cond  ((> cont tam) '())
         (else
          (cons (elemento N (elemento cont matrix)) (subElementoN-aux matrix N (+ 1 cont) tam) ))))

;Descripcion: Realiza una rotacion hacia la izquierda del cubo
;Entradas: Cubo(matriz con los colores del cubo)
;Salidas: retorna un cubo rotado
(define (rotarCubo cubo)
  (list (rotarIzquierdaMatriz(elemento 1 cubo)) (elemento 6 cubo)
        (rotarDerechaMatriz(elemento 3 cubo))
        (rotarDerechaMatriz(rotarDerechaMatriz(elemento 5 cubo))) (elemento 2 cubo) (rotarDerechaMatriz(rotarDerechaMatriz(elemento 4 cubo)))))

;Descripcion: Realiza una rotacion hacia la derecha del cubo
;Entradas: Cubo(matriz con los colores del cubo)
;Salidas: retorna un cubo rotado
(define (revertirCubo cubo)
  (rotarCubo(rotarCubo(rotarCubo cubo))))

;Descripcion: Retorna el numero especifico de fila o columna
;Entradas:  Movs(lista de movimientos)
;Salidas: el numero especifico de fila o columna
(define (obtenerNumero Movs)
  (string->number(string (car(cdr(string->list(symbol->string (car Movs))))))))

;Descripcion: Realiza un movimiento de columna
;Entradas: Cubo(matriz con los colores del cubo), fila, cont(contador para la cantidad caras que deben cambiar),
;Movs(lista de movimientos), primero(primera fila/columna que se va a cmabiar), cont2(para repetir un ciclo), dir(direccion)
;Salidas: el cubo con la columna actualizada 
(define (actualizar-columna Cubo fila cont Movs primero cont2 dir)
  (println (rotarCubo Cubo))
  (println (elemento (obtenerNumero Movs) (elemento 1 (rotarCubo Cubo))))
  (revertirCubo (actualizar-fila (rotarCubo Cubo)
                                 (obtenerNumero Movs)
                                 1
                                 Movs (elemento (obtenerNumero Movs) (elemento 1 (rotarCubo Cubo))) 1 dir)))


        
  
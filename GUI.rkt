#lang scheme
(require (lib "graphics.ss" "graphics"))
(require "Utilidades.rkt")
(provide iniciar)

(open-graphics)

;Obtener color: Retorna un color en inglés a partir del color en formato de letra 'X
;Entrada: letra en formato 'X
;Salida: Un color en inglés
(define (obtenerColor color)
  (cond ((equal? color 'A)
         "blue")
         ((equal? color 'B)
          "white"
        )
         ((equal? color 'V)
          "green"
        )
         ((equal? color 'Y)
          "yellow"
        )
         ((equal? color 'N)
          "orange"
        )
         ((equal? color 'R)
          "red"
        )
         ((equal? color 'Q)
          "black"
        )(else
          "magenta")
         ))


;Vértices de cubo

(define aX 253)
(define aY 143)

(define bX 472)
(define bY 38)

(define cX 750)
(define cY 120)

(define dX 532)
(define dY 225)

(define eX 750)
(define eY 450)

(define fX 532)
(define fY 553)

(define gX 253)
(define gY 471)

;Manejadores de movimiento de cubo

(define movX 1)
(define movY #t)

(define paso 1)

(define matriz '())

;Iniciar: Inicia la interfaz gráfica y envía los parámetros a las demás funciones
;Entrada: Tamaño del cubo/ Matriz con el Cubo/ Lista de movimientos

(define (iniciar num Cubo movs)
  (set! matriz Cubo)
  (arranque)
  (pintarCaras matriz num)
  (repintarLineas num)
  (click num matriz movs)
 
  )

;Click: Maneja el evento de clic dentro de la interfaz gráfica
;Entrada: Tamaño del cubo/ Matriz con el Cubo/ Lista de movimientos
;Salida: Un actualiza el cubo si se presiona un botón
(define (click num matriz movs)
  (cond
    (((get-mouse-click ventana)
      (cond ((and (> ( posn-x (query-mouse-posn ventana)) 91) (> ( posn-y (query-mouse-posn ventana)) 150)
                  (< ( posn-x (query-mouse-posn ventana)) 169) (< ( posn-y (query-mouse-posn ventana)) 213))
             (cond ((< paso (+ 1 (largo movs)))
                    (set! matriz (update num matriz (list (elemento paso movs))))
                     (set! paso (+ 1 paso))
                    ))
             
             )
            ((and (> ( posn-x (query-mouse-posn ventana)) 156) (> ( posn-y (query-mouse-posn ventana)) 484)
                  (< ( posn-x (query-mouse-posn ventana)) 295) (< ( posn-y (query-mouse-posn ventana)) 561))
             (cond (movY (set! movX (- movX 1)))
                   (else (set! movX (- movX 1)) )
               ) 
             )
            ((and (> ( posn-x (query-mouse-posn ventana)) 703) (> ( posn-y (query-mouse-posn ventana)) 476)
                  (< ( posn-x (query-mouse-posn ventana)) 842) (< ( posn-y (query-mouse-posn ventana)) 545))
             (cond (movY (set! movX (+ movX 1)))
                   (else (set! movX (+ movX 1)) )
               )
                                  )
            ((and (> ( posn-x (query-mouse-posn ventana)) 804) (> ( posn-y (query-mouse-posn ventana)) 230)
                  (< ( posn-x (query-mouse-posn ventana)) 940) (< ( posn-y (query-mouse-posn ventana)) 386))
             (set! movY (not movY))
             ))
      (revisarPos)
      (pintarCaras matriz num)
      (repintarLineas num)
      (click num matriz movs)
         )) 
   ))

;Pintar Caras: Pinta las caras a partir de un estado del cubo, ya sea frente, lado o reverso
;Entrada: Matriz con el cubo/ Tamaño del cub
;Salida: Cubo pintado
(define (pintarCaras matriz num)
  (cond ((and (= 1 movX) movY)
           (pintarCarafrente (elemento 1 matriz) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (elemento 2 matriz) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (elemento 5 matriz) num aX aY bX bY cX cY dX dY)
         )
        ((and (= 2 movX) movY)
           (pintarCarafrente (elemento 2 matriz) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (elemento 3 matriz) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (rotarDerechaMatriz(elemento 5 matriz)) num aX aY bX bY cX cY dX dY)
         )
        ((and (= 3 movX) movY)
           (pintarCarafrente (elemento 3 matriz) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (elemento 4 matriz) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (rotarDerechaMatriz(rotarDerechaMatriz(elemento 5 matriz))) num aX aY bX bY cX cY dX dY)
         )
        ((and (= 4 movX) movY)
           (pintarCarafrente (elemento 4 matriz) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (elemento 1 matriz) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (rotarIzquierdaMatriz(elemento 5 matriz)) num aX aY bX bY cX cY dX dY)
         )
        ((and (equal? 1 movX) (not movY))
           (pintarCarafrente (rotarDerechaMatriz(rotarDerechaMatriz(elemento 4 matriz))) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (rotarDerechaMatriz(rotarDerechaMatriz(elemento 3 matriz))) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (rotarDerechaMatriz(elemento 6 matriz)) num aX aY bX bY cX cY dX dY)
         )
        ((and (equal? 2 movX) (not movY))
           (pintarCarafrente (rotarDerechaMatriz(rotarDerechaMatriz(elemento 3 matriz))) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (rotarDerechaMatriz(rotarDerechaMatriz(elemento 2 matriz))) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (rotarDerechaMatriz(rotarDerechaMatriz(elemento 6 matriz))) num aX aY bX bY cX cY dX dY))
         
        ((and (equal? 3 movX) (not movY))
           (pintarCarafrente (rotarDerechaMatriz(rotarDerechaMatriz(elemento 2 matriz))) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (rotarDerechaMatriz(rotarDerechaMatriz(elemento 1 matriz))) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (rotarIzquierdaMatriz(elemento 6 matriz)) num aX aY bX bY cX cY dX dY)
         )
        ((and (equal? 4 movX) (not movY))
           (pintarCarafrente (rotarDerechaMatriz(rotarDerechaMatriz(elemento 1 matriz))) num aX aY dX dY fX fY gX gY)
           (pintarCaraArribaLado (rotarDerechaMatriz(rotarDerechaMatriz(elemento 4 matriz))) num dX dY cX cY eX eY fX fY)
           (pintarCaraArribaLado (elemento 6 matriz) num aX aY bX bY cX cY dX dY)
         )
        
        
        ))

;RevisarPos: Revisa que la posición del cubo siempre esté entre 1 y 4 para que vaya dando vueltas
;Salida: en caso de que la posición sea mayor que 4 la devuelve a 1 y si es menor que uno la devuelve a 4
(define (revisarPos)
  (cond((> movX 4)
        (set! movX 1))
       ((< movX 1)
        (set! movX 4))
       ))

;PintarCaraFrente: pinta la cara del frente del cubo
;Entrada: Matriz con el cubo/ Numero de caras/ Aristas del cubo
;Salida: Pinta la cara del frente del cubo
(define (pintarCarafrente lista num arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)
 
  (for ([k (in-range arista1X (* num arista2X) (/(- arista4X arista1X) num))]
        [m (in-range arista1Y (* num arista4Y) (/(- arista4Y arista1Y) num))]
        [o (in-range 0 num 1)] #:break (= o num))  
   (for ([i (in-range k (* num arista2X) (/(- arista2X arista1X) num))]
         [j (in-range m (* num arista2Y) (/(- arista2Y arista1Y) num))]
         [n (in-range 0 num 1)] #:break (= n num) ) (sleep 0.03)(repintarLineas num)
    ((draw-solid-polygon ventana) (list
                                   (make-posn i j)
                                   (make-posn (+ i (/ (- arista2X arista1X) num))
                                              (+ j (/ (- arista2Y arista1Y) num))) 
                                   (make-posn (+ (/ (- arista4X arista1X) num)
                                                 (+ i (/ (- arista2X arista1X) num)))
                                              (+ (/ (- arista4Y arista1Y) num) (+ j (/ (- arista2Y arista1Y) num))))
                                   (make-posn (+ i (/ (- arista4X arista1X) num))
                                              (+ j (/ (- arista4Y arista1Y) num)))) 
                                  (make-posn 0 0) (obtenerColor (elemento (+ 1 n) (elemento (+ 1 o) lista))))
     )
    )
    )

;PintarCaraArribaLado: Esta pinta la cara de arriba y la de un lado
;Entrada: Matriz con el cubo/ Numero de caras/ Aristas del cubo
;Salida: Cara del lado o de arriba pintada
(define (pintarCaraArribaLado lista num arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)

  (for ([k (in-range arista1X (* num arista2X) (/(- arista4X arista1X) num))]
        [m (in-range arista1Y (* num arista4Y) (/(- arista4Y arista1Y) num))]
        [o (in-range 0 num 1)] #:break (= o num))  
   (for ([i (in-range k (*(- arista2X arista1X) (* 2 num)) (/(- arista2X arista1X) num))]
         [j (in-range m (*(- arista2Y arista1Y) (* 2 num)) (/(- arista2Y arista1Y) num))]
         [n (in-range 0 num 1)] #:break (= n num) ) (sleep 0.03)(repintarLineas num)
    ((draw-solid-polygon ventana) (list 
                                   (make-posn i j)
                                   (make-posn (+ i (/ (- arista2X arista1X) num))
                                              (+ j (/ (- arista2Y arista1Y) num))) 
                                   (make-posn (+ (/ (- arista4X arista1X) num)
                                                 (+ i (/ (- arista2X arista1X) num)))
                                              (+ (/ (- arista4Y arista1Y) num) (+ j (/ (- arista2Y arista1Y) num))))
                                   (make-posn (+ i (/ (- arista4X arista1X) num))
                                              (+ j (/ (- arista4Y arista1Y) num))))
                                  (make-posn 0 0) (obtenerColor (elemento (+ 1 n) (elemento (+ 1 o) lista)))))
     )
    )
    



;RepintarLineas: repinta las líneas negras del cubo
;Entrada: numero de caras
;Salida: líneas negras del cubo
(define (repintarLineas num)
  (crearLineas num aX aY bX bY cX cY dX dY)
  (crearLineas num dX dY cX cY eX eY fX fY)
  (crearLineas num aX aY dX dY fX fY gX gY)
  (crearLineas num dX dY aX aY bX bY cX cY)
  (crearLineas num fX fY dX dY cX cY eX eY)
  (crearLineas num gX gY aX aY dX dY fX fY))


;CrearLineas: Toma las aristas de un cuadrado y crea un área cuadriculada a partir del tamaño del cubo
;Entrada: Tamaño del cubo/ Aristas del cuadrado
(define (crearLineas cant arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)
  (for ([i (in-range arista1Y (+ (/ (/ (- arista4Y arista1Y) cant) 3) arista4Y) (/ (- arista4Y arista1Y) cant))]
        [j (in-range arista2Y (+ (/ (/ (- arista4Y arista1Y) cant) 3) arista3Y) (/ (- arista3Y arista2Y) cant))]
        [k (in-range arista1X (+ 1 arista4X) (/ (- arista4X arista1X) cant))]
        [m (in-range arista2X (+ 1 arista3X) (/ (- arista3X arista2X) cant))])
    ((draw-solid-polygon ventana) (list
                                   (make-posn (- k 2) (- i 2))
                                   (make-posn (- m 2) (- j 2))
                                   (make-posn (+ m 2) (+ j 2))
                                   (make-posn (+ k 2) (+ i 2))) (make-posn 0 0) "black")) 
)
;Se crea la ventana
(define ventana '())

;Se envían los parámetros de la ventana para inciarla.
(define (arranque)
  (set! ventana (open-viewport "Rubik Simulator" 1000 600))
  ((draw-pixmap ventana) "./Imagenes/fondo.png" (make-posn 0 0)))





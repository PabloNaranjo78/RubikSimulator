#lang scheme
(require (lib "graphics.ss" "graphics"))
(require "Utilidades.rkt")
(provide iniciar)



(open-graphics)

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

(define (iniciar num matriz movs)
   
  (pintarCaras matriz num)
  (repintarLineas num)
  (click num matriz movs)
 
  )

(define (click num matriz movs)
  (cond
    (((get-mouse-click ventana)
      (cond ((and (> ( posn-x (query-mouse-posn ventana)) 91) (> ( posn-y (query-mouse-posn ventana)) 150)
                  (< ( posn-x (query-mouse-posn ventana)) 169) (< ( posn-y (query-mouse-posn ventana)) 213))
             (println "Update")
             
             )
            ((and (> ( posn-x (query-mouse-posn ventana)) 156) (> ( posn-y (query-mouse-posn ventana)) 484)
                  (< ( posn-x (query-mouse-posn ventana)) 295) (< ( posn-y (query-mouse-posn ventana)) 561))
             (println "Derecha")
             (cond (movY (set! movX (+ movX 1)))
                   (else (set! movX (+ movX 1)) )
               ) 
             )
            ((and (> ( posn-x (query-mouse-posn ventana)) 703) (> ( posn-y (query-mouse-posn ventana)) 476)
                  (< ( posn-x (query-mouse-posn ventana)) 842) (< ( posn-y (query-mouse-posn ventana)) 545))
             (println "Izquirda")
             (cond (movY (set! movX (+ movX 1)))
                   (else (set! movX (+ movX 1)) )
               )
                                  )
            ((and (> ( posn-x (query-mouse-posn ventana)) 804) (> ( posn-y (query-mouse-posn ventana)) 230)
                  (< ( posn-x (query-mouse-posn ventana)) 940) (< ( posn-y (query-mouse-posn ventana)) 386))
             (println "Arriba")
             (set! movY (not movY))
             ))
      (revisarPos)
      (pintarCaras matriz num)
      (println (list movX movY))
      (repintarLineas num)
      (click num matriz movs)
         )) 
   ))

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

(define (revisarPos)
  (cond((> movX 4)
        (set! movX 1))
       ((< movX 1)
        (set! movX 4))
       ))

(define (pintarCarafrente lista num arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)
 
  (for ([k (in-range arista1X (* num arista2X) (/(- arista4X arista1X) num))]
        [m (in-range arista1Y (* num arista4Y) (/(- arista4Y arista1Y) num))]
        [o (in-range 0 num 1)] #:break (= o num))  
   (for ([i (in-range k (* num arista2X) (/(- arista2X arista1X) num))]
         [j (in-range m (* num arista2Y) (/(- arista2Y arista1Y) num))]
         [n (in-range 0 num 1)] #:break (= n num) )
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

(define (pintarCaraArribaLado lista num arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)

  (for ([k (in-range arista1X (* num arista2X) (/(- arista4X arista1X) num))]
        [m (in-range arista1Y (* num arista4Y) (/(- arista4Y arista1Y) num))]
        [o (in-range 0 num 1)] #:break (= o num))  
   (for ([i (in-range k (*(- arista2X arista1X) (* 2 num)) (/(- arista2X arista1X) num))]
         [j (in-range m (*(- arista2Y arista1Y) (* 2 num)) (/(- arista2Y arista1Y) num))]
         [n (in-range 0 num 1)] #:break (= n num) ) 
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
    




(define (repintarLineas num)
  (crearLineas num aX aY bX bY cX cY dX dY)
  (crearLineas num dX dY cX cY eX eY fX fY)
  (crearLineas num aX aY dX dY fX fY gX gY)
  (crearLineas num dX dY aX aY bX bY cX cY)
  (crearLineas num fX fY dX dY cX cY eX eY)
  (crearLineas num gX gY aX aY dX dY fX fY))



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
(define ventana (open-viewport "Rubik Simulator" 1000 600))
((draw-pixmap ventana) "./Imagenes/fondo.png" (make-posn 0 0))





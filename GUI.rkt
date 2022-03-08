#lang scheme
(require (lib "graphics.ss" "graphics"))
(require "Utilidades.rkt")
(provide iniciar)



(open-graphics)

(define (obtnerColor color)
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

(define (iniciar num matriz)
 ; (println "iniciando")
   
  (pintarCaraArribaLado "white" num aX aY bX bY cX cY dX dY)
  
  (pintarCarafrente "green" num aX aY dX dY fX fY gX gY)
  (pintarCaraArribaLado "red" num dX dY cX cY eX eY fX fY)
  
  (repintarLineas num)

  (click num matriz)
  ;(iniciar num matriz) 
  )

(define (click num matriz)
  (cond
    (((get-mouse-click ventana) (println (list (posn-y (query-mouse-posn ventana)) (posn-x (query-mouse-posn ventana)))) (click num matriz)
         )) 
   ))

(define (pintarCarafrente lista num arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)

  (for ([k (in-range arista1X (* num arista2X) (/(- arista4X arista1X) num))]
        [m (in-range arista1Y (* num arista4Y) (/(- arista4Y arista1Y) num))]
        [o (in-range 0 num 1)] #:break (= o num))  
   (for ([i (in-range k (* num arista2X) (/(- arista2X arista1X) num))]
         [j (in-range m (* num arista2Y) (/(- arista2Y arista1Y) num))]
         [n (in-range 0 num 1)] #:break (= n num) ) (sleep 0.01)
    ((draw-solid-polygon ventana) (list
                                   (make-posn i j)
                                   (make-posn (+ i (/ (- arista2X arista1X) num)) (+ j (/ (- arista2Y arista1Y) num))) 
                                   (make-posn (+ (/ (- arista4X arista1X) num) (+ i (/ (- arista2X arista1X) num))) (+ (/ (- arista4Y arista1Y) num) (+ j (/ (- arista2Y arista1Y) num))))
                                   (make-posn (+ i (/ (- arista4X arista1X) num)) (+ j (/ (- arista4Y arista1Y) num)))) (make-posn 0 0) lista)
     )
    )
    )

(define (pintarCaraArribaLado lista num arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)

  (for ([k (in-range arista1X (* num arista2X) (/(- arista4X arista1X) num))]
        [m (in-range arista1Y (* num arista4Y) (/(- arista4Y arista1Y) num))]
        [o (in-range 0 num 1)] #:break (= o num))  
   (for ([i (in-range k (*(- arista2X arista1X) (* 2 num)) (/(- arista2X arista1X) num))]
         [j (in-range m (*(- arista2Y arista1Y) (* 2 num)) (/(- arista2Y arista1Y) num))]
         [n (in-range 0 num 1)] #:break (= n num) ) (sleep 0.01)
    ((draw-solid-polygon ventana) (list
                                   (make-posn i j)
                                   (make-posn (+ i (/ (- arista2X arista1X) num)) (+ j (/ (- arista2Y arista1Y) num))) 
                                   (make-posn (+ (/ (- arista4X arista1X) num) (+ i (/ (- arista2X arista1X) num))) (+ (/ (- arista4Y arista1Y) num) (+ j (/ (- arista2Y arista1Y) num))))
                                   (make-posn (+ i (/ (- arista4X arista1X) num)) (+ j (/ (- arista4Y arista1Y) num)))) (make-posn 0 0) lista)
     )
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



(iniciar 3 2)



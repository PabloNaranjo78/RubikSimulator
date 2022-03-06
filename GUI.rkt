#lang scheme
(require (lib "graphics.ss" "graphics"))
(provide iniciar)


(open-graphics)

;Vértices de cubo

(define aX 253)
(define aY 150)

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

(define (iniciar num)
  (display "iniciando")
  (crearLineas num aX aY bX bY cX cY dX dY)
  (crearLineas num dX dY cX cY eX eY fX fY)
  (crearLineas num aX aY dX dY fX fY gX gY)

  (crearLineas num dX dY aX aY bX bY cX cY)
  (crearLineas num fX fY dX dY cX cY eX eY)
  (crearLineas num gX gY aX aY dX dY fX fY)
  
  )


(define ventana (open-viewport "Rubik Simulator" 1000 600))

(define (crearLineas cant arista1X arista1Y arista2X arista2Y arista3X arista3Y arista4X arista4Y)
  (for ([i (in-range arista1Y (+ (/ (/ (- arista4Y arista1Y) cant) 3) arista4Y) (/ (- arista4Y arista1Y) cant))]
        [j (in-range arista2Y (+ (/ (/ (- arista4Y arista1Y) cant) 3) arista3Y) (/ (- arista3Y arista2Y) cant))]
        [k (in-range arista1X (+ 1 arista4X) (/ (- arista4X arista1X) cant))]
        [m (in-range arista2X (+ 1 arista3X) (/ (- arista3X arista2X) cant))])
    (sleep 0.05)
    ((draw-solid-polygon ventana) (list
                                   (make-posn (- k 2) (- i 2))
                                   (make-posn (- m 2) (- j 2))
                                   (make-posn (+ m 2) (+ j 2))
                                   (make-posn (+ k 2) (+ i 2))) (make-posn 0 0) "black"))
)

(iniciar 30)
#lang scheme
(require "Utilidades.rkt")

(define matriz '( ( (V V V)
                    (B N Y)
                    (B N A) )

                  ( (B R A)
                    (V V V)
                    (Y Y Y) )

                  ( (B B B)
                    (B R Y)
                    (V R Y) ) 

                  ( (V N Y) 
                    (A A A)
                    (A A A) ) 

                  ( (N B R) 
                    (N B R)
                    (N V R) )

                  ( (R R R)
                    (Y Y A) 
                    (N N N) )  

          ( (N N N)
            (N N N) 
            (N N N) )))

(define (rotarCubo cubo)
  (list (rotarIzquierdaMatriz(elemento 1 cubo)) (elemento 6 cubo)
        (rotarDerechaMatriz(elemento 3 cubo))
        (rotarDerechaMatriz(rotarDerechaMatriz(elemento 5 cubo))) (elemento 2 cubo) (rotarDerechaMatriz(rotarDerechaMatriz(elemento 4 cubo))))
  )
(define (revertirCubo cubo)
  (rotarCubo(rotarCubo(rotarCubo cubo))))

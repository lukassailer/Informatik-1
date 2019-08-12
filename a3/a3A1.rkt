;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname IU3) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
(define pi 3.1415)

(define quadrat
  (lambda (n)
    (* n n)))

; (a)
(* 2 pi)
;~>[eval-lit(2)],[eval-id(pi)]
;(* 2 3.1415)
;~>[eval-id(*)],[eval-lit(3.1415)]
;(#< procedure :*> 2 3.1415)
;[apply-prim(*)]
;6.283

; (b)
(quadrat (+ 4 2))
;~>[eval-id(quadrat)]
;((lambda (n) (* n n))(+ 4 2))
;~>[eval-id(+)],[eval-lit(4)],[eval-lit(2)]
;((lambda (n) (* n n))(#<procedure:+> 4 2))
;~>apply-prim(+)
;((lambda (n) (* n n))6)
;~>[eval-λ],[eval-lit(6)]
;((lambda (n) (* n n))6)
;~>[apply-λ]
;(* 6 6)
;~>[eval-id(*)]
;(#<procedure:*> 6 6)
;~>[apply-prim(*)]
;36

; (c)
((lambda (pi) (* 2 pi)) pi)
;~>[eval-id(pi)]
;((lambda (pi) (* 2 pi))3.1415)
;~>[eval-λ],[eval-lit(3.1415)]
;((lambda (pi) (* 2 pi))3.1415)
;~>[apply-λ]
;(* 2 3.1415)
;~>[eval-id(*)],[eval-lit(3.1415)]
;(#<procedure:*> 2 3.1415)
;~>[apply-prim(*)
;6.283

; (d)
((lambda (a) a)
(+ ((lambda (a) (+ a 2)) 3) 2))
;~>[eval-λ]
;((lambda (a) a)(+ ((lambda (a) (+ a 2)) 3) 2))
;~>[apply-λ]
;((lambda (a) a)(+ (+ 3 2) 2))
;~>[eval-id(+)],[eval-lit(3)],[eval-lit(2)]
;((lambda (a) a)(+ (#<procedure:+> 3 2) 2))
;~>[apply-prim]
;((lambda (a) a)(+ 5 2))
;~>[eval-id(+)],[eval-lit(5)],[eval-lit(2)]
;((lambda (a) a)(#<procedure:+> 5 2))
;~>[apply-prim]
;((lambda (a) a)7)
;~>[eval-λ]
;((lambda (a) a)7)
;~>[apply-λ]
;7

; (e)
((lambda (x) x) (lambda (x) x))
;~>[eval-λ]
;((lambda (x) x) (lambda (x) x))
;~>[apply-λ]
;(lambda (x) x)
;~>[eval-λ]
;(lambda (x) x)
;~>[apply-λ]
;#<procedure>
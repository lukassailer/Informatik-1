;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname a3A3) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
(define heiner-or
  (lambda (test-1 test-2)
   (if test-1
    #t
    test-2)))

(heiner-or (= 10 10) (> 2 5))
;~>[eval-id(heiner-or)]
;(lambda (test-1 test-2) (if test-1 #t test-2)) (= 10 10) (> 2 5))
;~>[eval-id(=)], [eval-id(>)]
;(lambda (test-1 test-2) (if test-1 #t test-2)) (#<procedure:<=> 10 10) (#<procedure:>> 2 5))
;~>[apply-prim(=)], [apply-prim(>)],[eval-lit(10)],[eval-lit(10)],[eval-lit(2)],[eval-lit(5)]
;(lambda (test-1 test-2) (if test-1 #t test-2)) #t #f)
;~>[eval-λ]
;(lambda (test-1 test-2) (if test-1 #t test-2)) #t #f)
;~>[apply-λ]
;(if #t #t #f)
;#t


(or (= 10 10) (> 2 5)) ;(> 2 5) ist ausgegraut, es wird nicht ausgeführt
;~>[eval-id(=)] nach eval-or Reduktionsregeln
;(or (#<procedure:<=> 10 10) (> 2 5))
;~>[apply-prim(=)]
;(or #t (>2 5))
;~>[eval-or] da <e'1> = #t wird <e2> nicht reduziert
;#t


;Das heiner-or errechnet auch das Ergebnis für den zweiten Test, wenn dies eigentlich nicht nötig wäre (weil test1 = #t) und verschwendet damit Rechenleistung.
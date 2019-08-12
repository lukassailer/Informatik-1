;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname a3A2) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
(: less-zero? (number -> boolean))
(define less-zero?
  (lambda (x)
   (< x 0)))

(: f (number -> boolean))
(define f
  (lambda (y)
   (>= y 11)))

(: g (boolean boolean -> boolean))
(define g
  (lambda (a b)
   (not b) ))

(: greater-equal-zero? (number -> boolean))
(define greater-equal-zero?
  (lambda (x)
   (>= x 0)))

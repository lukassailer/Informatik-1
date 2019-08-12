;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname A3) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;A3
(define f
   (lambda (x)
      (i (g x) (h (g x)))))

;a
(define f
  (lambda (x)
    ((lambda (y)
       (i y (h y))
       (g x)))))

;b) Wir berechnen (g x) einmal, geben den Wert dann in die Lambda Funktion weiter wo dieser als y zwischengespeichert ist
;   und dort wird der Rest der Prozedur ausgeführt

;c)
(define f
  (lambda (x)
    (let (gx (g x))
      (i gx (h gx)))))
      



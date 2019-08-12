;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname A2) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm")))))
;A2 Luhn-Check

;sum Berechnet die summe einer Liste von Zahlen
(: sum ((list-of number) -> number))
(check-expect (sum (list 1 2 3 4 5 6 7 8 9)) 45)
(check-expect (sum empty) 0)
(check-expect (sum (list 5)) 5)
(define sum
  (lambda (l)
    (letrec ((sum-worker (lambda (xs acc)
                           (match xs
                             (empty acc)
                             ((make-pair f empty) (+ acc f))
                             ((make-pair f r) (sum-worker r (+ acc f)))))))
      (sum-worker l 0))))


;digits zerlegt ein nätürliche Zahl x in ihre einzelnen Ziffern und gibt diese in einer Liste in der Reihenfolge Niederwertigste Ziffer zu Höchstwertigster Ziffer zurück
(: digits (natural -> (list-of natural)))
(check-expect (digits 1024) (list 4 2 0 1))
(check-expect (digits 0) (list 0))
(check-expect (digits 133742069) (list 9 6 0 2 4 7 3 3 1))
(check-expect (digits 9) (list 9))
(check-expect (digits 10) (list 0 1))
(define digits
  (lambda (x)
    (letrec ((digits-worker (lambda (y acc)
                              (if (>= y 10)
                                  (digits-worker (quotient y 10) (append acc (list (remainder y 10))))
                                  (append acc (list y))))))
      (digits-worker x empty))))
     

;double-every-other-number Verdoppelt jede Zweite Zahl einer Liste und gibt die daraus folgende Liste zurück
(: double-every-other-number ((list-of number) -> (list-of number)))
(check-expect (double-every-other-number (list 1 2 3 4 5)) (list 1 4 3 8 5))
(check-expect (double-every-other-number (list 5 6)) (list 5 12))
(check-expect (double-every-other-number (list 1)) (list 1))
(check-expect (double-every-other-number empty) empty)
(define double-every-other-number
  (lambda (xs)
    (letrec ((worker (lambda (zs acc)
                       (match zs
                         (empty acc)
                         ((make-pair f empty) (append acc (list f)))
                         ((make-pair f (make-pair rf rr)) (worker rr (append acc (list f (* rf 2)))))))))
      (worker xs empty))))


;map-digits wendet auf jedes Element einer Liste natürlicher Zahlen die Funktion digits an und gibt eine Liste von Listen mit den einzelnen Ziffern zurück
(: map-digits ((list-of natural) -> (list-of (list-of natural))))
(check-expect (map-digits (list 2 13 9)) (list (list 2) (list 3 1) (list 9)))
(check-expect (map-digits empty) empty)
(define map-digits
  (lambda (xs)
    (map digits xs)))


;concat bekommt eine Liste von Listen und Hängt diese in eine Liste zusammen
(: concat ((list-of (list-of %a)) -> (list-of %a)))
(check-expect (concat (list (list 1 2) (list 3 4) (list 5))) (list 1 2 3 4 5))
(check-expect (concat empty) empty)
(check-expect (concat (list (list 3))) (list 3))
(define concat
  (lambda (xs)
    (letrec ((concat-worker (lambda (zs acc)
                              (match zs
                                (empty acc)
                                ((make-pair f r) (concat-worker r (append acc f)))))))
      (concat-worker xs empty))))


;luhn-check führt einen sog. Luhn-check auf eine natürliche Zahl x aus. War er erfolgreich wird #t zurückgegeben, wenn nicht #f
(: luhn-check (natural -> boolean))
(check-expect (luhn-check 5678) #t)
(check-expect (luhn-check 6789) #f)
(define luhn-check
  (lambda (x)
    (zero? (remainder
            (sum
             (concat
              (map-digits
               (double-every-other-number
                (digits x)))))
            10))))
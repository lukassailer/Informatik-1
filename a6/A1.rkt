;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname A1) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; Ein polymorphes Paar (pair-of ‹t1› ‹t2›) besteht aus
; - erster Komponente (first)
; - zweiter Komponente (rest)
; wobei die Komponenten jeweils Werte der Signaturen ‹t1› bzw. ‹t2› sind:
(: make-pair (%a %b -> (pair-of %a %b)))
(: pair? (any -> boolean))
(: first ((pair-of %a %b) -> %a))
(: rest  ((pair-of %a %b) -> %b))
(define-record-procedures-parametric pair pair-of
  make-pair
  pair?
  (first
   rest))

; Polymorphe Listen der Signatur (list-of ‹t›):
(define list-of
  (lambda (t)
    (signature (mixed empty-list
                      (pair-of t (list-of t))))))

;last Gibt das letzte Element einer beliebiegen Liste zurück, wenn die Liste leer ist kommt der Fehler "Liste ist leer"
(: last ((list-of %a) -> %a))
(check-expect (last (make-pair 1 (make-pair 2 (make-pair 3 empty)))) 3)
(check-error (last empty) "Liste ist leer")
(check-expect (last (make-pair 42 empty)) 42)
(define last
  (lambda (l)
    (match l
      (empty (violation "Liste ist leer"))
      ((make-pair f empty) f)
      ((make-pair f ls) (last ls)))))

;elem? Überprüft ob die übergebene Zahl ein Element der Liste ist.
(: elem? (number (list-of number) -> boolean))
(check-expect (elem? 1 (make-pair 1 (make-pair 2 empty))) #t)
(check-expect (elem? 2 (make-pair 1 (make-pair 2 empty))) #t)
(check-expect (elem? 3 (make-pair 1 (make-pair 2 empty))) #f)
(check-expect (elem? 1 empty) #f)
(define elem?
  (lambda (x l)
    (match l
      (empty #f)
      ((make-pair f ls) (if (= f x)
                            #t
                            (elem? x ls))))))

;max-list Gibt die größte Zahl der übergebenen Liste zurück
(: max-list ((list-of natural) -> natural))
(check-expect (max-list (make-pair 1 (make-pair 2 empty))) 2)
(check-expect (max-list (make-pair 42 (make-pair 1 empty))) 42)
(check-expect (max-list (make-pair 1 (make-pair 2 (make-pair 32 empty)))) 32)
(check-error (max-list empty) "Liste ist leer")
(define max-list
  (lambda (l)
    (match l
      (empty (violation "Liste ist leer"))
      ((make-pair f empty) f)
      ((make-pair f ls) (if (>= f (max-list ls))
                            f
                            (max-list ls))))))


;init Gibt Liste aller Elemente außer das letzte einer nicht leeren Menge
(: init ((list-of %a) -> (list-of %a)))
(check-expect (init (make-pair 1 (make-pair 42 empty))) (make-pair 1 empty))
(check-expect (init (make-pair "a" (make-pair "c" (make-pair "b" empty)))) (make-pair "a" (make-pair "c" empty)))
(check-error (init empty) "Liste ist leer")
(define init
  (lambda (l)
    (match l
      (empty (violation "Liste ist leer"))
      ((make-pair _ empty) empty)
      ((make-pair f ls) (make-pair f (init ls))))))


;all-equal? Überprüft ob alle elemente einer Liste gleich sind wenn Liste leer #t
(: all-equal? ((list-of number) -> boolean))
(check-expect (all-equal? (make-pair 1 (make-pair 1 (make-pair 1 empty)))) #t)
(check-expect (all-equal? (make-pair 1 (make-pair 2 (make-pair 1 empty)))) #f)
(check-expect (all-equal? empty) #t)
(check-expect (all-equal? (make-pair 1 empty )) #t)
(define all-equal?
  (lambda (l)
    (match l
      (empty #t)
      ((make-pair f empty) #t)
      ((make-pair f ls) (and (= f (last l)) (all-equal? ls))))))



;replicate Schreibt in eine Liste n-mal den beliebigen Wert v
(: replicate (natural %a -> (list-of %a)))
;SANDSTORM LYRICS ersten 4 Zeilen:
(check-expect (replicate 1 "Duuuuuuuuuuuuuuuuuuuuuuun") (make-pair "Duuuuuuuuuuuuuuuuuuuuuuun" empty))
(check-expect (replicate 14 "DUN") (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" empty)))))))))))))))
(check-expect (replicate 13 "DUN") (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" (make-pair "DUN" empty))))))))))))))
(check-expect (replicate 1 "BOOM") (make-pair "BOOM" empty))

(check-expect (replicate 0 "OHNOO") empty)
(define replicate
  (lambda (n v)
    (if (> n 0)
        (make-pair v (replicate (- n 1) v))
        empty)))
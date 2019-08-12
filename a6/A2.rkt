;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname A2) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
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




;insert-sorted Fügt eine reele Zahl sortiert in eine sortierte Liste ein
(: insert-sorted (real (list-of real) -> (list-of real)))
(check-expect (insert-sorted 23 (make-pair 3 (make-pair 12 (make-pair 45 (make-pair 113 empty)))))
              (make-pair 3 (make-pair 12 (make-pair 23 (make-pair 45 (make-pair 113 empty))))))
(check-expect (insert-sorted 115 (make-pair 3 (make-pair 12 (make-pair 45 (make-pair 113 empty)))))
              (make-pair 3 (make-pair 12 (make-pair 45 (make-pair 113 (make-pair 115 empty))))))
(check-expect (insert-sorted 42 empty) (make-pair 42 empty))
(define insert-sorted
  (lambda (x l)
    (match l
      (empty (make-pair x empty))
      ((make-pair f ls) (if (> x f)
                            (make-pair f (insert-sorted x ls))
                            (make-pair x (make-pair f ls)))))))


;sort-list Sortiert eine Liste  reeler Zahlen
(: sort-list ((list-of real) -> (list-of real)))
(check-expect (sort-list (make-pair 113 (make-pair 3 (make-pair 45 (make-pair 12 (make-pair 23 (make-pair 4 (make-pair 45 empty))))))))
              (make-pair 3 (make-pair 4 (make-pair 12 (make-pair 23 (make-pair 45 (make-pair 45 (make-pair 113 empty))))))))
(check-expect (sort-list empty)
              empty)
(check-expect (sort-list (make-pair 42 (make-pair 24 empty)))
              (make-pair 24 (make-pair 42 empty)))
(define sort-list
  (lambda (l)
    (match l
      (empty l)
      ((make-pair f empty) (make-pair f empty))
      ((make-pair f ls) (insert-sorted f (sort-list ls))))))
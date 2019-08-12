;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname A1) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm")))))
;tuple beschreibt ein Tupel aaus zwei beliebigen Elementen
(: make-tuple (%a %b -> (tuple-of %a %b)))
(: tuple? (any -> boolean))
(: tuple-1 ((tuple-of %a %b) -> %a))
(: tuple-2  ((tuple-of %a %b) -> %b))
(define-record-procedures-parametric tuple tuple-of
  make-tuple
  tuple?
  (tuple-1
   tuple-2))

;split-list teilt eine Liste in zwei abwechselnd befüllte Listen und fügt diese in ein Tupel zusammen
(: split-list ((list-of %a) -> (tuple-of (list-of %a) (list-of %a))))
(check-expect (split-list (list 1 2 3 4 5)) (make-tuple (list 1 3 5) (list 2 4)))
(check-expect (split-list empty) (make-tuple empty empty))
(check-expect (split-list (list "x")) (make-tuple (list "x") empty))
(define split-list
  (lambda (l)
    (letrec ((split-worker (lambda (l acc)
                             (match l
                               (empty acc)
                               ((make-pair f empty) (append acc (list f)))
                               ((make-pair f (make-pair rf empty)) (append acc (list f)))
                               ((make-pair f (make-pair rf rr)) (split-worker rr (append acc (list f))))))))
      (make-tuple (split-worker l empty) (if (empty? l)
                                             empty
                                             (split-worker (rest l) empty))))))


;weave-lists macht das gleich wie split-list nur umgekehrt
(: weave-lists ((tuple-of (list-of %a) (list-of %a)) -> (list-of %a)))
(check-expect (weave-lists (make-tuple (list 1 3 5 6) (list 2 4))) (list 1 2 3 4 5 6))
(check-expect (weave-lists (make-tuple (list 1 2 3) empty)) (list 1 2 3 ))
(check-expect (weave-lists (make-tuple empty (list "a" "b"))) (list "a" "b"))
(check-expect (weave-lists (make-tuple (list 1 3 5 6) (list 2 4 4 4))) (list 1 2 3 4 5 4 6 4))
(check-expect (weave-lists (make-tuple empty empty)) empty)
(define weave-lists
  (lambda (t)
    (letrec ((weave-worker (lambda (l1 l2)(match l1
                                            (empty l2)
                                            ((make-pair f1 r1) (match l2
                                                                 (empty l1)
                                                                 ((make-pair f2 r2) (append (list f1 f2) (weave-worker r1 r2)))))))))
      (weave-worker (tuple-1 t) (tuple-2 t)))))


;check
(check-property
 (for-all ((xs (list-of natural)))
   (expect (weave-lists (split-list xs)) xs)))
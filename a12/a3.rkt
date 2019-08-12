;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname a3) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;tuple prozedur
(: make-tuple (%a %b -> (tuple-of %a %b)))
(: tuple? (any -> boolean))
(: tuple-first ((tuple-of %a %b) -> %a))
(: tuple-second ((tuple-of %a %b) -> %b))
(define-record-procedures-parametric tuple tuple-of
  make-tuple
  tuple?
  (tuple-first
   tuple-second))

;teilt eine Liste an allen möglichen Stellen in zwei Listen und fügt diese zu einem Tupel zusammen
(: splits ((list-of %a) -> (list-of (tuple-of (list-of %a) (list-of %a)))))
(check-expect (splits (list 1 2 3))
              (list (make-tuple empty (list 1 2 3))
                    (make-tuple (list 1) (list 2 3))
                    (make-tuple (list 1 2) (list 3))
                    (make-tuple (list 1 2 3) empty)))
(check-expect (splits (list 1 2))
              (list (make-tuple empty (list 1 2))
                    (make-tuple (list 1) (list 2))
                    (make-tuple (list 1 2) empty)))
(check-expect (splits empty)
              (list (make-tuple empty empty)))
(define splits
  (lambda (xs)
    (letrec ((splits-worker
              (lambda (xs ys t)
                (match xs
                  (empty ys)
                  ((make-pair f r) (splits-worker r
                                                  (append ys (list (make-tuple
                                                                    (append (tuple-first t) (list f))
                                                                    r)))
                                                  (make-tuple (append (tuple-first t) (list f))
                                                              r)))))))
      (append (list (make-tuple empty xs))
              (splits-worker xs empty (make-tuple empty empty))))))

;gibt alle möglicher Permutationen einer Liste aus
(: permutations ((list-of %a) -> (list-of (list-of %a))))
(check-expect (permutations (list 1 2 3))
              (list (list 1 2 3)
                    (list 2 1 3)
                    (list 2 3 1)
                    (list 1 3 2)
                    (list 3 1 2)
                    (list 3 2 1)))
(check-expect (permutations (list 1 2))
              (list (list 1 2)
                    (list 2 1)))
(check-expect (permutations (list "a"))
              (list (list "a")))
(check-expect (permutations empty)
              (list empty))
(define permutations
  (lambda (xs)
    (letrec ((sxs (splits xs))
             (list-worker (lambda (x xs acc)
                            (match xs
                              (empty (make-pair (append acc (list x)) empty))
                              ((make-pair f r) (make-pair (append acc (list x) xs) (list-worker x r (append acc (list f))))))))
             (map-worker (lambda (x xs)
                           (match xs
                             (empty empty)
                             ((make-pair f r) (append (list-worker x f empty) (map-worker x r)))))))
      (match (if (< (length sxs) 2)
                 empty
                 (list-ref sxs 1))
        (empty (list empty))
        ((make-tuple (make-pair ff empty)
                     empty)                (list (list ff)))
        ((make-tuple (make-pair ff empty)
                     (make-pair sf empty)) (list-worker ff (list sf) empty))
        ((make-tuple (make-pair ff fr)
                     ys)                   (map-worker ff (permutations ys)))))))
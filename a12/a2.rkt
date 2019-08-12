;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname a2) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
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

;teilt eine Liste in einen Tuple aus zwei abwechseln befüllten Listen
(: split ((list-of %a) -> (tuple-of (list-of %a) (list-of %a))))
(check-expect (split (list 1 4 7 2)) (make-tuple (list 1 7) (list 4 2)))
(check-expect (split (list 1 4 7)) (make-tuple (list 1 7) (list 4)))
(check-expect (split (list "a" "b")) (make-tuple (list "a") (list "b")))
(define split
  (lambda (xs)
    (letrec ((split-worker
              (lambda (xs t)
                (match xs
                  (empty t)
                  ((make-pair f empty) (make-tuple (append (tuple-first t) (list f)) (tuple-second t)))
                  ((make-pair f (make-pair rf rr)) (split-worker rr (make-tuple (append (tuple-first t) (list f)) (append (tuple-second t) (list rf)))))))))
      (split-worker xs (make-tuple empty empty)))))

;fügt bezüglich des Kriteriums lt? sortierte Listen xs und ys zu einer ebenfalss danach sortien Liste zusammen
(: merge-by ((%a %a -> boolean) (list-of %a) (list-of %a) -> (list-of %a)))
(check-expect (merge-by < (list 1 7) (list 2 4)) (list 1 2 4 7))
(check-expect (merge-by < (list 100 200) (list 1 2)) (list 1 2 100 200))
(check-expect (merge-by < empty (list 1 2)) (list 1 2))
(define merge-by
  (lambda (lt? xs ys)
    (letrec ((ctrl-v
              (lambda (lt? xs y)
                (match xs
                  (empty (list y))
                  ((make-pair f r) (if (lt? y f)
                                       (make-pair y xs)
                                       (make-pair f (ctrl-v lt? r y))))))))
    (match ys
      (empty xs)
      ((make-pair f r) (merge-by lt? (ctrl-v lt? xs f) r))))))

;sortiert eine liste unter Zuhilfenahme der oberen Funktionen (anhand des Kriteriums lt?)
(: mergesort ((%a %a -> boolean) (list-of %a) -> (list-of %a)))
(check-expect (mergesort < (list 1 4 7 2)) (list 1 2 4 7))
(check-expect (mergesort < (list 1)) (list 1))
(check-expect (mergesort < empty) empty)
(define mergesort
  (lambda (lt? xs)
    (let ((t-xs (split xs)))
    (merge-by lt? (tuple-first t-xs) (tuple-second t-xs)))))
    
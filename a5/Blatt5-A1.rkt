;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname Blatt5-A1) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(: make-t1 (natural (mixed t2 natural) natural -> t1))
(: t1? (any -> boolean))
(: t1-a (t1 -> natural))
(: t1-b (t1 -> (mixed t2 natural)))
(: t1-c (t1 -> natural))
(define-record-procedures t1
  make-t1
  t1?
  (t1-a
   t1-b
   t1-c))

(: make-t2 (natural natural -> t2))
(: t2? (any -> boolean))
(: t2-d (t2 -> natural))
(: t2-e (t2 -> natural))
(define-record-procedures t2
  make-t2
  t2?
  (t2-d
   t2-e))

(: f (t1 -> integer))
(define f
  (lambda (x)
    (if (natural? (t1-b x))
        (+ (t1-a x)
           (t1-b x)
           (t1-c x))
        (if (and (= (t2-e (t1-b x)) 1)
                 (= (t2-d (t1-b x)) 1))
            10
            (- (+ (t2-d (t1-b x))
                  (t2-e (t1-b x)))
               (t1-a x))))))



;f aber mit pattern matching und ohne conditional expressions
(: f* (t1 -> integer))
(define f*
  (lambda (x)
    (match x
      [(make-t1 a b c) (match b
                         [(make-t2 1 1) 10]
                         [(make-t2 d e) (- (+ d e) a)]
                         [_ (+ a b c)])])))

;testet für ganz viele mögliche werte für a b c d e ob f*(x)=f(x)
(check-property (for-all ((a natural)
                          (b natural)
                          (c natural)
                          (d natural)
                          (e natural))
                  (and (= (f (make-t1 a b c))
                          (f* (make-t1 a b c)))
                       (= (f (make-t1 a (make-t2 d e) c))
                           (f* (make-t1 a (make-t2 d e) c))))))
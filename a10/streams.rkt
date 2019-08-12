;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname streams) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; ----------------------------------------------------------------------
; Streams (unendliche Ströme von Elementen gleicher Signatur)

; "Versprechen", ein Wert der Signatur t liefern zu können
(define promise
  (lambda (t) 
    (signature (-> t))))

; "Einlösung" (Auswertung) des Versprechens p
(: force ((promise %a) -> %a))
(define force
  (lambda (p) 
    (p)))

; Polymorphe Paare (isomorph zu `pair')
(: make-cons (%a %b -> (cons-of %a %b)))
(: head ((cons-of %a %b) -> %a))
(: tail ((cons-of %a %b) -> %b))
(define-record-procedures-parametric cons cons-of
  make-cons 
  cons?
  (head
   tail))

; Streams mit Elementen der Signatur t
(define stream-of
  (lambda (t) 
    (signature (cons-of t (promise cons)))))

(: from (number -> (stream-of number)))
(define from
  (lambda (n) 
    (make-cons n (lambda () (from (+ n 1))))))

; Erzeuge die ersten n Elemente des Strom str (Stream -> Liste)
(: stream-take (natural (stream-of %a) -> (list-of %a)))
(check-expect (stream-take 5 (from 1)) (list 1 2 3 4 5)) 
(check-expect (stream-take 0 (from 1)) empty)
(define stream-take
  (lambda (n str)
    (match str
      ((make-cons hd tl)
       (if (= n 0)
           empty
           (make-pair hd (stream-take (- n 1) (force tl))))))))


;;; EIGENE LOESUNG AB HIER

;akzeptiert einen belieben Wert und gibt einen endlosen Strom von diesem zurück
(: const-stream (%a -> (stream-of %a)))
(check-expect (stream-take 1 (const-stream "A")) (list "A"))
(check-expect (stream-take 5 (const-stream "A")) (list "A" "A" "A" "A" "A"))
(define const-stream
  (lambda (x)
    (make-cons x (lambda () (const-stream x)))))

;ones ist ein Stream aus 1
(define ones
  (const-stream 1))

;wendet eine Prozedur auf die Elemente eines Stroms an
(: stream-map ((%a -> %b) (stream-of %a) -> (stream-of %b)))
(check-expect (stream-take 5 (stream-map (lambda (x) (+ x 2)) ones)) (list 3 3 3 3 3))
(check-expect (stream-take 5 (stream-map (lambda (x) (> x 1)) ones)) (list #f #f #f #f #f))
(define stream-map
  (lambda (f s)
    (make-cons (f (head s)) (lambda () (stream-map f (force (tail s)))))))

;produziert einen stream aus wiederholter anwendung von f auf das erste Element besteht
(: stream-iterate ((%a -> %a) %a -> (stream-of %a)))
(check-expect (stream-take 5 (stream-iterate (lambda (x) (+ x 2)) 1)) (list 1 3 5 7 9))
(check-expect (stream-take 5 (stream-iterate (lambda (x) (string-append x x)) "-")) (list "-" "--" "----" "--------" "----------------"))
(define stream-iterate
  (lambda (f s)
    (make-cons s (lambda () (stream-iterate f (f s))))))

;liefert das erste Elememt aus dem konvergierenen Strom s, dass sich um weniger als d von seinem Vorgänger unterscheidet
(: stream-converge (real (stream-of real) -> real))
(check-expect (stream-converge 10 (stream-iterate (lambda (x) (/ x 10)) 100)) 1)
(check-within (stream-converge 0.3 (stream-iterate (lambda (x) (/ x 10))
                                                   100) )
              0.01
              0.00001)
(define stream-converge
  (lambda (d s)
    (if (< (abs (- (head s) (head (force (tail s))))) d)
        (head (force (tail s)))
        (stream-converge d (force (tail s))))))

;approximiert die quadratwurzel einer Zahl, die sich weniger als Delta Δ von ihrem Vorgänger unterscheidet
(: approx-sqrt (real real -> real))
(check-within (approx-sqrt 15 0.01) 3.872 0.001)
(check-within (approx-sqrt 25 0.01) 5 0.001)
(define approx-sqrt
  (lambda (a delta)
    (stream-converge delta (stream-iterate (lambda (x) (/ (+ x (/ a x)) 2)) a))))
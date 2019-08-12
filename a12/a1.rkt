;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname a1) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
(: unlines ((list-of string) -> string))
(define unlines
  (lambda (ys)
    (fold "" (lambda (x xs) (string-append x "\n" xs)) ys)))

(: print ((list-of string) -> %nothing))
(define print
  (lambda (ss)
    (write-string (unlines ss))))

;gibt ein Gebirge mit n Bergen aus, wobei der größte eine Höhe von n hat
(: mountain-peaks (natural -> (list-of string)))
(check-expect (mountain-peaks 1)
              (list "/\\"))
(check-expect (mountain-peaks 2)
              (list " /\\" "/\\ \\"))
(define mountain-peaks
  (lambda (n)
    (if (zero? n)
        empty
        (mountain-spacer (- n 1) (mountain-worker (- n 1) "/\\")))))

;baut Gebirge (ohne Leerzeichen)
(: mountain-worker (natural string -> (list-of string)))
(define mountain-worker
  (lambda (n k)
    (cond [(zero? n) (list k)]
          [(odd? n) (make-pair k (mountain-worker (- n 1) (string-append k " \\")))]
          [(even? n) (make-pair k (mountain-worker (- n 1) (string-append "/ " k)))]
          [else empty])))

;fügt Leerzeichen zu einem Gebirge hinzu
(: mountain-spacer (natural (list-of string) -> (list-of string)))
(define mountain-spacer
  (lambda (n xs)
    (letrec ((n-spaces
              (lambda (n)
                (cond [(< n 1) ""]
                      [(= n 1) " "]
                      [else (string-append " " (n-spaces (- n 1)))]))))
      (match xs
        (empty empty)
        ((make-pair f r) (make-pair (string-append (n-spaces n) f) (mountain-spacer (if (zero? n)
                                                                                        0
                                                                                        (- n 1))
                                                                                    r)))))))


(print (mountain-peaks 1))
(print (mountain-peaks 4))
(print (mountain-peaks 5))
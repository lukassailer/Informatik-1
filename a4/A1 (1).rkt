;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname A1) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;calendar-date beschreibt die Daten eines Datums (day, month und year)
(define-record-procedures calendar-date
  make-calendar-date
  calendar-date?
  (calendar-date-day
   calendar-date-month
   calendar-date-year))

(: make-calendar-date (natural natural integer -> calendar-date))
(: calendar-date? (any -> boolean))
(: calendar-date-day (calendar-date -> natural))
(: calendar-date-month (calendar-date -> natural))
(: calendar-date-year (calendar-date -> integer))


(: Lukas calendar-date)
(define Lukas
  (make-calendar-date 21 10 1997))

(: Luca calendar-date)
(define Luca
  (make-calendar-date 1 3 1999))


;calendar-date-ok? Überprüft ob ein Datum sinnvoll ist 
(: calendar-date-ok? (calendar-date -> boolean))
(check-expect (calendar-date-ok? Lukas) #t)
(check-expect (calendar-date-ok? Luca) #t)
(check-expect (calendar-date-ok? (make-calendar-date 0 1 2018)) #f)
(check-expect (calendar-date-ok? (make-calendar-date 30 2 -333)) #f)
(check-expect (calendar-date-ok? (make-calendar-date 0 11 111)) #f)
(define calendar-date-ok?
  (lambda (cd)
    (and (< 0 (calendar-date-month cd) 13)
         (cond [(or (= (calendar-date-month cd) 4)
                    (= (calendar-date-month cd) 6)
                    (= (calendar-date-month cd) 9)
                    (= (calendar-date-month cd) 11)) (< 0 (calendar-date-day cd) 31)]
               [(= (calendar-date-month cd) 2) (< 0 (calendar-date-day cd) 30)]
               [else (< 0 (calendar-date-day cd) 32)]))))


;calendar-date-ok/leap-year? Verhält sich wie calendar-date-ok? beachtet aber auch noch Schaltjahre
(: calendar-date-ok/leap-year? (calendar-date -> boolean))
(check-expect (calendar-date-ok/leap-year? (make-calendar-date 29 2 400)) #t)
(check-expect (calendar-date-ok/leap-year? (make-calendar-date 29 2 200)) #f)
(check-expect (calendar-date-ok/leap-year? (make-calendar-date 29 2 2004)) #t)
(check-expect (calendar-date-ok/leap-year? Luca) #t)
(define calendar-date-ok/leap-year?
  (lambda (cd)
    (if (and (calendar-date-ok? cd)
             (or (= (modulo (calendar-date-year cd) 400) 0)
                 (and (> (modulo (calendar-date-year cd) 100) 0)
                      (= (modulo (calendar-date-year cd) 4) 0))))
        #t
        (if (= (calendar-date-month cd) 2) (< (calendar-date-day cd) 29)
            #t))))
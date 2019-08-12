;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname A2) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
;A2

;Daten- und Recorddefinition für Kreise
(define-record-procedures
  kreis
  make-kreis
  kreis?
  (kreis-radius
   kreis-farbe))
(: make-kreis (real string -> kreis))
(: kreis? (any -> boolean))
(: kreis-radius (kreis -> real))
(: kreis-farbe (kreis -> string))


;Daten- und Recorddefinition für Rechtecke
(define-record-procedures
  rechteck
  make-rechteck
  rechteck?
  (rechteck-höhe
   rechteck-breite
   rechteck-farbe))
(: make-rechteck (real real string -> rechteck))
(: rechteck? (any -> boolean))
(: rechteck-höhe (rechteck -> real))
(: rechteck-breite (rechteck -> real))
(: rechteck-farbe (rechteck -> string))


;Daten- und Recorddefinition für Dreiecke
(define-record-procedures
  dreieck
  make-dreieck
  dreieck?
  (dreieck-seitenlänge
   dreieck-farbe))
(: make-dreieck (real string -> dreieck))
(: dreieck? (any -> boolean))
(: dreieck-seitenlänge (dreieck -> real))
(: dreieck-farbe (dreieck -> string))


;Signatur für beliebeige Form Kreis, Rechteck oder Dreieck
(define shape
  (signature (mixed kreis dreieck rechteck)))

;Beispiel Formen
(define kreis0
  (make-kreis 2 "red"))

(define dreieck0
  (make-dreieck 2 "green"))

(define rechteck0
  (make-rechteck 2 2 "blue"))

(define kreis1
  (make-kreis 20 "red"))

(define dreieck1
  (make-dreieck 50 "green"))

(define rechteck1
  (make-rechteck 30 50 "blue"))

;Wert von Pi
(define pi 3.1415)

;shape-area Gibt die Fläche einer Beliebigen Form Kreis, Dreieck oder Rechteck zurück
(: shape-area (shape -> real))
(check-within (shape-area kreis0) (* 4 pi) 0.1)
(check-within (shape-area dreieck0) 1.73 0.1)
(check-expect (shape-area rechteck0) 4)
(define shape-area
  (lambda (shape)
    (cond ((kreis? shape) (*(quadrieren (kreis-radius shape)) pi))
          ((dreieck? shape) (*  (/ (quadrieren (dreieck-seitenlänge shape)) 4) (sqrt 3)))
          ((rechteck? shape) (* (rechteck-höhe shape) (rechteck-breite shape))))))


;quadrieren Quadriert die Zahl x
(: quadrieren (real -> real))
(check-expect (quadrieren 2) 4)
(check-expect (quadrieren -2) 4)
(check-expect (quadrieren 1) 1)
(check-expect (quadrieren 0) 0)
(define quadrieren
  (lambda (x)
    (* x x)))

;scale-lenght berechnet die Länge des Balkens der Waage
(: scale-length (image image real -> real))
(check-expect (scale-length (draw-shape kreis1) (draw-shape dreieck1) 40) 130)
(define scale-length
  (lambda (image1 image2 w2)
    (+ (image-width image1) w2 (image-width image2))))


;draw-shape zeichnet eine beliebige Form Kreis, Dreieck oder Rechteck
(: draw-shape (shape -> image))
(check-expect (draw-shape kreis1) (circle 20 "solid" "red"))
(check-expect (draw-shape rechteck1) (rectangle 50 30 "solid" "blue"))
(check-expect (draw-shape dreieck1) (triangle 50 "solid" "green"))
(define draw-shape
  (lambda (shape)
    (cond ((kreis? shape) (circle (kreis-radius shape) "solid" (kreis-farbe shape)))
          ((dreieck? shape) (triangle (dreieck-seitenlänge shape) "solid" (dreieck-farbe shape))) 
          ((rechteck? shape) (rectangle (rechteck-breite shape) (rechteck-höhe shape) "solid" (rechteck-farbe shape))))))


;draw-scale Zeichnet zwei Formen auf einer Waage mit dem abstand von 40px
(: draw-scale (shape shape -> image))
(define draw-scale
  (lambda (shape1 shape2)
    (rotate  (rotation-angle (shape-area shape1) (shape-area shape2))
             (above 
              (beside/align
               "bottom" (draw-shape shape1) (empty-scene 40 0) (draw-shape shape2) )
              (line (scale-length (draw-shape shape1) (draw-shape shape2) 40) 0 "black")))))


;rotation-angle Berechnet den Winkel der Waage anhand der wirkenden Kräfte
(: rotation-angle (real real -> real))
(check-expect (rotation-angle 10 10) 0)
(check-expect (rotation-angle 10 20) -45)
(check-expect (rotation-angle 40 20) 45)
(define rotation-angle
  (lambda (a1 a2)
    (* 90 (if (> a1 a2)
              (- 1 (/ a2 a1))
              (+ -1 (/ a1 a2))))))

(draw-scale rechteck1 dreieck1)
(draw-scale kreis1 rechteck1)
(draw-scale kreis1 dreieck1)

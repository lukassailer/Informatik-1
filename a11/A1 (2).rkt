;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname A1) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.ss" "teachpack" "deinprogramm")))))
; Vorgegebene Definitionen: Blatt 11 vom 18.1.19
; Aufgabe 1: Binärbäume


; Ein Knoten (node) besitzt
; - einen linken Zweig (left-branch),
; - eine Markierung (label) und
; - einen rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch  ((node-of %a %b %c) -> %a))
(: node-label        ((node-of %a %b %c) -> %b))
(: node-right-branch ((node-of %a %b %c) -> %c))

(define-record-procedures-parametric node node-of
  make-node
  node?
  (node-left-branch
   node-label
   node-right-branch))


; Ein leerer Baum (empty-tree) besitzt
; keine weiteren Eigenschaften
(: make-empty-tree (-> empty-tree))

(define-record-procedures empty-tree
  make-empty-tree
  empty-tree?
  ())


; Signatur für Binärbäume (btree-of t) mit Markierungen der Signatur t
; (im linken/rechten Zweig jedes Knotens findet sich jeweils wieder
; ein Binärbaum)
(define btree-of
  (lambda (t)
    (signature (mixed empty-tree
                      (node-of (btree-of t) t (btree-of t))))))
;                              \__________/   \__________/
;                                  ↑               ↑
;                                 zweifache Rekursion, s. (list-of t)


; Erzeuge einen leeren Baum
(: the-empty-tree empty-tree)
(define the-empty-tree (make-empty-tree))


; Erzeuge einen Blattknoten, d.h. beide Zweige sind je ein leerer Baum
(: make-leaf (%a -> (btree-of %a)))
(define make-leaf
  (lambda (x)
    (make-node the-empty-tree x the-empty-tree)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eigene Lösung ab hier

; Beispielbaum t1
(: t1 (btree-of natural))
(define t1
  (make-node (make-leaf 2)
             1
             (make-leaf 3)))
; Beispielbaum t2
(: t2 (btree-of natural))
(define t2
  (make-node (make-leaf 2)
             4
             (make-leaf 3)))
; Beispielbaum t3
(: t3 (btree-of natural))
(define t3
  (make-node (make-leaf 2)
             4
             (make-leaf 1)))
; Beispielbaum t4
(: t4 (btree-of natural))
(define t4
  (make-node (make-node (make-leaf 1)
                        2
                        (make-leaf 3))
             4
             (make-leaf 5)))
                        
;(a) Ermittelt die minimale markierung eines binär baums bei einem leeren Baum ist das Ergebniss +inf.0
(: btree-min ((btree-of real) -> real))
(check-within (btree-min t1) 1 0.01)
(check-within (btree-min t2) 2 0.01)
(check-within (btree-min t3) 1 0.01)
(check-within (btree-min t4) 1 0.01)
(check-range (btree-min the-empty-tree) +inf.0 +inf.0)
(define btree-min
  (lambda (t)
    (match t
      ((make-empty-tree) +inf.0)
      ((make-node l x r) (min (btree-min l) x (btree-min r))))))

;(b) Ermittelt die maximale markierung eines binär baums bei einem leeren Baum ist das Ergebniss -inf.0
(: btree-max ((btree-of real) -> real))
(check-within (btree-max t1) 3 0.01)
(check-within (btree-max t2) 4 0.01)
(check-within (btree-max t3) 4 0.01)
(check-within (btree-max t4) 5 0.01)
(check-range (btree-max the-empty-tree) -inf.0 -inf.0)
(define btree-max
  (lambda (t)
    (match t
      ((make-empty-tree) -inf.0)
      ((make-node l x r) (max (btree-max l) x (btree-max r))))))
;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname A3) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.ss" "teachpack" "deinprogramm")))))
; Vorgegebene Definitionen: Blatt 11 vom 18.1.19
; Aufgabe 3: Suchbäume


; Ein Knoten (node) besteht aus
; - einem linken Zweig (left-branch)
; - einer Markierung (label) und
; - einem rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch ((node-of %a %b %c) -> %a))
(: node-label ((node-of %a %b %c) -> %b))
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


; Falte Baum t bzgl. z und c
(: btree-fold (%b (%b %a %b -> %b) (btree-of %a) -> %b))

(check-expect (btree-fold 5 (lambda (left label right) 5) the-empty-tree) 5) 
(check-expect (btree-fold 1 (lambda (left label right) (+ left right)) (make-leaf "l1")) 2) 
(check-expect (btree-fold 1 (lambda (left label right) (+ left right)) (make-node (make-leaf "l1") "n1" (make-leaf "l2"))) 4)

(define btree-fold
  (lambda (z c t)
    (match t
      ((make-empty-tree) z)
      ((make-node left label right)
       (c (btree-fold z c left)
          label
          (btree-fold z c right))))))



; Exemplarische Suchbäume

; Korrekter Suchbaum
(: t1 (btree-of integer))
(define t1
  (make-node (make-node (make-leaf -4)
                        1
                        (make-leaf 3))
             12
             (make-node (make-leaf 13)
                        16
                        (make-leaf 20))))

; Kaputter Suchbaum (-4 < 1 und rechts(1) = -4)
(: t2 (btree-of integer))
(define t2
  (make-node (make-node (make-leaf 10)
                        1
                        (make-leaf -4))
             12
             (make-node (make-leaf 13)
                        16
                        (make-leaf 200))))

; Kaputter Suchbaum (10 doppelt)
(: t3 (btree-of integer))
(define t3
  (make-node (make-node (make-leaf -4)
                        1
                        (make-leaf 10))
             12
             (make-node (make-leaf 10)
                        16
                        (make-leaf 200))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eigene Lösung ab hier

;(a) Stellt fest ob ein Baum t ein Suchbaum ist
(: search-tree? ((btree-of real) -> boolean))
(check-expect (search-tree? t1) #t)
(check-expect (search-tree? t2) #f)
(define search-tree?
  (lambda (t)
    (letrec ((search-worker (lambda (t)
                               (match t
                                 ((make-empty-tree) #t)
                                 ((make-node (make-empty-tree) x (make-empty-tree)) #t)
                                 ((make-node l x r) (and (< (if (empty-tree? l)
                                                                -inf.0
                                                                (node-label l))
                                                            x
                                                            (if (empty-tree? r)
                                                                +inf.0
                                                                (node-label r)))
                                                         (search-worker l)
                                                         (search-worker r)))))))
      (search-worker t))))
             
                                                                     
                           
      

(define searchtree-of
  (lambda (t)
    (signature (predicate search-tree?))))

;(b) Entscheidet ob sich eine Marikierung x in einem Suchbaum befindet
(: searchtree-member? (natural (searchtree-of integer) -> boolean))
(check-expect (searchtree-member? 20 t1) #t)
(check-expect (searchtree-member? 42 t1) #f)
(check-expect (searchtree-member? 1 t1)  #t)
(define searchtree-member?
  (lambda (x t)
    (match t
      ((make-empty-tree) #f)
      ((make-node (make-empty-tree) y (make-empty-tree)) (= x y))
      ((make-node l y r) (if (= x y)
                             #t
                             (if (< x y)
                                 (searchtree-member? x l)
                                 (searchtree-member? x r)))))))


;(c) Fügt eine Markierung y in einen Suchbaum t an der richtigen Stelle ein
(: searchtree-insert (integer (searchtree-of integer) -> (searchtree-of integer)))
(check-expect (searchtree-insert 42 the-empty-tree) (make-leaf 42))
(check-expect (searchtree-insert 42 t1)
              (make-node (make-node (make-leaf -4)
                                    1
                                    (make-leaf 3))
                         12
                         (make-node (make-leaf 13)
                                    16
                                    (make-node the-empty-tree
                                               20
                                               (make-leaf 42)))))
(check-expect (searchtree-insert 2 t1)
              (make-node (make-node (make-leaf -4)
                                    1
                                    (make-node (make-leaf 2)
                                               3
                                               the-empty-tree))
                         12
                         (make-node (make-leaf 13)
                                    16
                                    (make-leaf 20))))
(check-expect (searchtree-insert 12 t1) t1)
(define searchtree-insert
  (lambda (y t)
    (match t
      ((make-empty-tree) (make-leaf y))
      ((make-node l x r) (if (= x y)
                             t
                             (if (< y x)
                                 (make-node (searchtree-insert y l) x r)
                                 (make-node l x (searchtree-insert y r))))))))


;(d) Wandelt eine Liste von Elementen in einen Suchbaum um
(: list->searchtree ((list-of integer) -> (searchtree-of integer)))
(check-expect (list->searchtree (list 3 -4 20 13 16 1 12)) t1)
(define list->searchtree
  (lambda (l)
    (fold the-empty-tree searchtree-insert l)))



;(e) Entfernt eine Markierung y aus einem Suchbaum t
(: searchtree-delete (integer (searchtree-of integer) -> (searchtree-of integer)))
(check-expect (searchtree-delete 1 (list->searchtree (list 1 2 3)))
              (list->searchtree (list 2 3)))
(check-expect (searchtree-delete 4 (list->searchtree (list 1 2 3)))
              (list->searchtree (list 1 2 3)))
(define searchtree-delete
  (lambda (y t)
    (match t
      ((make-empty-tree) the-empty-tree)
      ((make-node (make-empty-tree) x (make-empty-tree)) (if (= y x)
                                                             the-empty-tree
                                                             (make-leaf x)))
      ((make-node l x r) (cond ((< y x) (make-node (searchtree-delete y l) x r))
                               ((> y x) (make-node l x (searchtree-delete y r)))
                               ((empty-tree? l) r)
                               ((empty-tree? r) l)
                               (else (make-node l (node-label r) (searchtree-delete (node-label r) r))))))))
                               




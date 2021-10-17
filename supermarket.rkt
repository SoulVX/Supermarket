#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (cond
    ((null? counters) '())
    ((= (counter-index (car counters)) index) (append (list (f (car counters))) (update f (cdr counters) index)))
    (else (append (list (car counters)) (update f (cdr counters) index)))))

(define tt+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [tt (+ (counter-tt C) minutes)])]))))

(define et+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [et (+ (counter-et C) minutes)])]))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C)) (struct-copy counter C [queue (enqueue (cons name items) (counter-queue C))]
                                                           [tt (+ (counter-tt C) items)]
                                                           [et (+ (counter-et C) items)])
                                         (struct-copy counter C [queue (enqueue (cons name items) (counter-queue C))]
                                                           [tt (+ (counter-tt C) items)]))))

(define (min-helper f counters index_min val_min)
    (cond
    ((null? counters) (cons index_min val_min))
    ((< (f (car counters)) val_min) (min-helper f (cdr counters) (counter-index (car counters)) (f (car counters))))
    (else (min-helper f (cdr counters) index_min val_min))))

(define (min-tt counters)
  (min-helper counter-tt counters -1 +inf.0))
(define (min-et counters)
  (min-helper counter-et counters -1 +inf.0))

(define (get-delay tt queue)
  (if (queue-empty? queue) tt
      (get-delay (- tt (cdr (top queue))) (dequeue queue))))

(define (remove-first-from-counter C)   ; testată de checker
  (if (queue-empty? (dequeue (counter-queue C))) (struct-copy counter C [tt 0]
                                                           [et 0]
                                                           [queue empty-queue])
  (struct-copy counter C [tt (- (- (counter-tt C) (cdr (top (counter-queue C)))) (get-delay (counter-tt C) (counter-queue C)))]
                         [et (cdr (top (dequeue (counter-queue C))))]
                         [queue (dequeue (counter-queue C))])))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (if (< (counter-tt C) minutes) (struct-copy counter C [tt 0] [et 0])
        (struct-copy counter C [tt (- (counter-tt C) minutes)]
                               [et (- (counter-et C) minutes)]))))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve-helper requests fast-counters slow-counters exits)

  (define (is-in-list? index list)
    (cond
      ((null? list) #f)
      ((= index (counter-index (car list))) #t)
      (else (is-in-list? index (cdr list)))))
  
  (define delay-counter
    (λ (minutes)
      (λ (C)
        (match C
          [(counter index tt et queue)
           (struct-copy counter C [et (+ (counter-et C) minutes)] [tt (+ (counter-tt C) minutes)])]))))

  (define (delay-option index minutes)
    (if (is-in-list? index fast-counters) (serve-helper (cdr requests) (update (delay-counter minutes) fast-counters index) slow-counters exits)
        (serve-helper (cdr requests) fast-counters (update (delay-counter minutes) slow-counters index) exits)))

  (define (get-average-tt list sum nr)
    (if (null? list) (cons (/ sum nr) nr)
        (get-average-tt (cdr list) (+ sum (counter-tt (car list))) (+ 1 nr))))

  (define (ensure-option ttmed)
    (if (< ttmed (car (get-average-tt (append fast-counters slow-counters) 0 0))) (serve-helper requests fast-counters (append slow-counters (list (empty-counter (+ 1 (cdr (get-average-tt (append fast-counters slow-counters) 0 0)))))) exits)
        (serve-helper (cdr requests) fast-counters slow-counters exits)))
  
  (define (add-option name n-items)
    (if (and (<= n-items ITEMS) (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters))))
        (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters exits)
        (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) exits))) 

  (define (time-helper-fast minutes fast-counters new-fast-counters exits)
    (cond
      ((null? fast-counters) (time-helper-slow minutes (reverse new-fast-counters) slow-counters '() exits))
      ((or (> (counter-et (car fast-counters)) minutes) (zero? (counter-tt (car fast-counters))))
          (time-helper-fast minutes (cdr fast-counters) (cons ((pass-time-through-counter minutes)(car fast-counters)) new-fast-counters) exits))
      ((zero? (+ (queue-size-l (counter-queue (car fast-counters))) (queue-size-r (counter-queue (car fast-counters)))))
          (time-helper-fast minutes (cdr fast-counters) (cons (empty-counter (counter-index (car fast-counters))) new-fast-counters) exits))
      ((zero? (counter-tt (remove-first-from-counter (car fast-counters))))
          (time-helper-fast minutes (cdr fast-counters) (cons (remove-first-from-counter (car fast-counters)) new-fast-counters)
                            (append exits (list (cons (counter-index (car fast-counters)) (car (top (counter-queue (car fast-counters)))))))))
      (else (time-helper-fast minutes (cdr fast-counters)
                            (cons
                               (struct-copy counter (remove-first-from-counter (car fast-counters))
                               [tt (- (counter-tt (remove-first-from-counter (car fast-counters))) (- minutes (cdr (top (counter-queue (car fast-counters))))))]
                               [et (- (counter-et (remove-first-from-counter (car fast-counters))) (- minutes (cdr (top (counter-queue (car fast-counters))))))])
                             new-fast-counters)
                            (append exits (list (cons (counter-index (car fast-counters)) (car (top (counter-queue (car fast-counters)))))))))))

  (define (time-helper-slow minutes new-fast-counters slow-counters new-slow-counters exits)
    (cond
      ((null? slow-counters) (serve-helper (cdr requests) new-fast-counters (reverse new-slow-counters) exits))
      ((or (> (counter-et (car slow-counters)) minutes) (zero? (counter-tt (car slow-counters))))
          (time-helper-slow minutes new-fast-counters (cdr slow-counters) (cons ((pass-time-through-counter minutes)(car slow-counters)) new-slow-counters) exits))
      ((zero? (+ (queue-size-l (counter-queue (car slow-counters))) (queue-size-r (counter-queue (car slow-counters)))))
          (time-helper-slow minutes new-fast-counters (cdr slow-counters) (cons (empty-counter (counter-index (car slow-counters))) new-slow-counters) exits))
      ((zero? (counter-tt (remove-first-from-counter (car slow-counters))))
          (time-helper-slow minutes new-fast-counters (cdr slow-counters) (cons (remove-first-from-counter (car slow-counters)) new-slow-counters)
                            (append exits (list (cons (counter-index (car slow-counters)) (car (top (counter-queue (car slow-counters)))))))))
      (else (time-helper-slow minutes new-fast-counters (cdr slow-counters)
                            (cons
                               (struct-copy counter (remove-first-from-counter (car slow-counters))
                               [tt (- (counter-tt (remove-first-from-counter (car slow-counters))) (- minutes (cdr (top (counter-queue (car slow-counters))))))]
                               [et (- (counter-et (remove-first-from-counter (car slow-counters))) (- minutes (cdr (top (counter-queue (car slow-counters))))))])
                             new-slow-counters)
                            (append exits (list (cons (counter-index (car slow-counters)) (car (top (counter-queue (car slow-counters)))))))))))

  (define (time-option number)
    (time-helper-fast number fast-counters '() exits))
  
  (if (null? requests) (append (list exits) fast-counters slow-counters)
      (match (car requests)
        [(list 'delay index minutes) (delay-option index minutes)]
        [(list 'ensure minutes)      (ensure-option minutes)]
        [(list name n-items)         (add-option name n-items)]
        [number                      (time-option number)]
        )))

  (define (format-output-helper counters)
    (cond
      ((null? counters) '())
      ((queue-empty? (counter-queue (car counters))) (format-output-helper (cdr counters)))
      (else (append (list (cons (counter-index (car counters)) (counter-queue (car counters)))) (format-output-helper (cdr counters))))))

  (define (format-output output)
    (cons (car output) (format-output-helper (cdr output))))

(define (serve requests fast-counters slow-counters)
  (format-output (serve-helper requests fast-counters slow-counters '())))


        

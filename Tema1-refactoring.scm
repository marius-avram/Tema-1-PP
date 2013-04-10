(include "Reflection.scm")

;Params: -fun este functia evaluata
;        -refactoring-pack este o lista de patternuri
(define (refactor-code fun refactoring-pack)
  ;Face refactor cu un sigur pattern pana nu mai poate fi aplicat
  (define (refactor-once fun-lambda refactoring-packet prev-lambda)
    (begin ;(display fun-lambda) (display "*****\n")
    (if (equal? fun-lambda prev-lambda)
        fun-lambda
        (refactor-once (replace-matching-seq (input-pattern refactoring-packet)
                                             (output-pattern refactoring-packet) 
                                             fun-lambda '()) refactoring-packet fun-lambda))))
  ;Face refactor cu un pachet de patternuri
  (define (refactor-for-all fun refactoring-pack)
    (if (null? refactoring-pack)
        fun
        (refactor-for-all (refactor-once fun (car refactoring-pack) '()) (cdr refactoring-pack))))
  
  ;Face refactor pana nici un refactoring-pack nu mai poate fi aplicat
  (define (refactor-until-done fun-lambda refactoring-pack res-lambda) 
    (if (equal? fun-lambda res-lambda)
        res-lambda
        (refactor-until-done (refactor-for-all fun-lambda refactoring-pack) refactoring-pack fun-lambda)))
  
  (cadr (refactor-until-done (get-function-name-body fun) refactoring-pack '())))
  

; Misc

; Testeaza daca un simbol incepe cu un anumit caracter
; Ex (symbol-starts-with? #\? '?variable) => #t
(define (symbol-starts-with? first-char x)
    (and (symbol? x)
         (let ((s (symbol->string x)))
           (and (>= (string-length s) 1)
                (equal? first-char (string-ref s 0))))))


; Testeaza ca argumentul este o entitate primitiva 
; (care nu mai poate fi descompusa)
(define (atom? x)
  (or (symbol? x)
      (number? x)
      (char? x)
      (string? x)
      (boolean? x)))

; Determina patternul introdus care trebuie sa fie gasit 
; intr-o anumita functie definita cu define-r
(define (input-pattern p) 
 (if (equal? (caar p) 'WHEN)
     (cadar p)
     'INVALID-PATTERN))

;(input-pattern '((WHEN (lambda (?x) (?f ?x))) (THEN ?f)))

; Determina patternul cu care trebuie inlocuit input-pattern
(define (output-pattern p)
  (if (equal? (caadr p) 'THEN)
      (cdadr p)
      'INVALID-PATTERN))

;(output-pattern '((WHEN (lambda (?x) (?f ?x))) (THEN ?f)))


; Face append pe doua liste care contin perechi (simbol,valoare)
; Daca se intalnesc perechi cu aceeasi cheie dar cu valori
; diferite va da eroare. In caz contrar face append.
(define (unique-append L1 L2)
  ;verifica daca un element cu o cheie data are alta 
  ;valoare decat cea dintr-o lista. Daca nu gaseste cheia
  ;returneaza true
  (define (check-unique-elem L1 pair2)
  (equal? (filter (lambda (pair) 
            (if (equal? (car pair) (car pair2))
                (equal? (cadr pair) (cadr pair2))
                true)) L1 ) L1))
  
  ;Elimina dintr-o lista cheia data
  (define (get-unique L1 pair2)
    (filter (lambda (pair)
              (not (and (equal? (car pair) (car pair2)) 
                        (equal? (cadr pair) (cadr pair2))))) L1))
  
  
  (define (do-append L1 L2)
    (if (equal? (filter (lambda (pair) (check-unique-elem L1 pair)) L2) L2)
        (append L1 (filter  
                    (lambda (pair)
                      (equal? (get-unique L1 pair) L1)) L2))
        'error))
  
  (do-append L1 L2))

;(display "Unique append FTW ")
;(unique-append '((?x x) (?y y)) '((?x z) (?z z)))

;Verifica daca o secventa din pattern este corespunzator unei
;variabile conditionate.
(define (is-conditional? seq)
  (if (and (list? seq) (> (length seq) 1)) 
      (and (equal? (car seq) ':conditional) (symbol-starts-with? #\? (cadr seq)))
      false))

;Verifica daca un pattern este corespunzator unei evaluari 
(define (is-evaluation? seq)
  (if (and (list? seq) (> (length seq) 1))
      (equal? (car seq) ':evaluation)
      false))

;Returneaza dintr-un pattern de tip evaluare doar expresia ce trebuie evaluata 
(define (get-evaluation-body p)
  (if (is-evaluation? p)
      (cadr p)
      'error))

;Se da o secventa de pattern corespunzator unei variabile conditionate, 
;si secventa din functia propriu-zisa. Se verifica daca predicatul e adevarat.
(define (verify-conditional pseq expr)
  (if (is-conditional? pseq)
      ((eval (caddr pseq)) expr)
      false))

;Verifica daca un pattern este o abstractizare 
(define (is-abstraction? seq) 
  (and (list? seq) (equal? (car seq) ':abstraction)))

;Primeste o lista de simboluri legate, care este de forma 
;'((?sym1 val1) (?sym2 val2) ...). Si incearca sa mai adauge 
;un simbol. Daca acesta exista deja atunci nu il mai adauga. 
;Insa in cazul in care simbolul exista, dar se incearca adaugarea 
;unei noi valori va returna 'error.
 (define (make-binding L sym value Lrez)
  
  (define (make-variable-binding L sym value Lrez)
    (if (null? L) 
        (cons (list sym value) Lrez)
        (if (equal? (caar L) sym)
            (if (not (equal? (cadar L) value)) 
                'error
                (make-variable-binding (cdr L) sym value (cons (car L) Lrez)))
            (make-variable-binding (cdr L) sym value (cons (car L) Lrez)))))
  
  (define (make-conditional-binding L sym value Lrez)
    (make-variable-binding L 
                           (car (filter (lambda (e) 
                                     (symbol-starts-with? #\? e)) sym))
                           value 
                           Lrez))
  
  (define (binding-type L sym value Lrez)
    (cond ((symbol-starts-with? #\? sym) (make-variable-binding L sym value Lrez))
          ((is-conditional? sym) (make-conditional-binding L sym value Lrez))
          (else 'error)))
  
  (binding-type L sym value Lrez))

  

; Determina daca un pattern se potriveste cu o lista data. 
; Lista este o parte a unei functiei definita cu define-r
; Returneaza variabilele legate
(define matches-with-bindings
  (lambda (p L binds)
    ;daca abele secvente in acelasi timp s-au terminat inseamna ca au avut
    ;dimensiune egala si ca atat patternul cat si secventa au fost 
    ;identice
    (if (and (null? p) (null? L))
        binds
        ;daca secventa data s-a terminat dar patternul e null
        ;sau daca patternul s-a terminat si secventa data nu =>esec
        (if (or (and (not (null? p)) (null? L)) (and (null? p) (not (null? L))))
            'error
            (let ((first (car p)) (second (car L))) 
                  ;daca in pattern nu avem ca prim element o lista si daca
                  ;daca s-a intalnit o variabila ce trebuie legata se sare peste ea     
                  (if (or (symbol-starts-with? #\? first)
                          (and (is-conditional? first) (verify-conditional first second)))
                      (let ((bind-list (make-binding binds first second '())))
                      (if (equal? bind-list 'error)
                          'error
                          (matches-with-bindings (cdr p) (cdr L) bind-list)))
                      ;daca in pattern avem ca prim element o lista trebuie ca si in 
                      ;secventa cautata sa fie o lista, daca nu e => esec
                      (if (list? first)
                          (if (list? second)
                              (let ((match-head (matches-with-bindings first second binds))
                                    (match-tail (matches-with-bindings (cdr p) (cdr L) binds)))
                              (if (or (equal? match-head 'error) (equal? match-tail 'error))
                                  'error
                                  (unique-append match-head match-tail)))
                              'error) 
                          ;daca secventele coincid continua
                          (if (equal? first second)
                              (matches-with-bindings (cdr p) (cdr L) binds)
                              'error))))))))

;Face append astfel incat sa se mentina parantezarea
(define (bracket-app L e)
    
  (define (mlist L)
      (if (list? L)
          L
          (cons L '())))
  
  (if (null? e) 
      (if (list? L)
          (if (and (= (length L) 1) (equal? (car L) 'lambda))
              (append L (list e))
              L)
          (list L))
      (reverse (cons e (reverse (mlist L))))))




;Primeste patternul de output si il returneaza tot pe acesta insa 
;inlocuieste variabilele din contextul dat. Pentru a realiza acest 
;lucru primeste si lista de perechi (simbol, valoare).
(define (apply-pattern p L Lrez)
  ;Cazul in care avem un pattern de tip variabila simpla sau 
  ;conditionata
  (define (apply-variable-pattern p L Lrez)
    (if (null? p)
        Lrez
        (let ((head (car p)))
          (if (or (symbol-starts-with? #\? head)
                  (and (is-conditional? first) (verify-conditional first second)))
              (apply-variable-pattern (cdr p) L (bracket-app Lrez (cadar (filter (lambda (e) (equal? (car e) head)) L))))
              (if (list? head)
                  (append (bracket-app  (apply-variable-pattern Lrez L '()) (apply-variable-pattern head L '())) 
                          (apply-variable-pattern (cdr p) L '()))
                  (apply-variable-pattern (cdr p) L (bracket-app Lrez head)))))))
  
  ;Cazul in care avem un pattern de tip evaluare
  (define (apply-evaluation-pattern p L Lrez)
    (list (eval (apply-variable-pattern p L Lrez))))
  
  ;In functie de pattern alege functia interna corespunzatoare
  (define (choose-apply-pattern p L Lrez)
    (if (is-evaluation? (car p))
        (apply-evaluation-pattern (get-evaluation-body (car p)) L Lrez)
        (apply-variable-pattern p L Lrez)))
  (choose-apply-pattern p L Lrez))

;Verifica daca intr-o lista se gaseste un pattern cautat.
;Va verifica toate sublistele.
(define (check-compatibility inp subbody level)
  (if (null? subbody)
      -1
      (let ((head (car subbody)))
      (if (list? head)
          (if (equal? (matches-with-bindings inp head '()) 'error)
              (max (check-compatibility inp head (+ level 1)) (check-compatibility inp (cdr subbody) level))
              level)
          (check-compatibility inp (cdr subbody) level)))))
              

;Parcurge lista in totalitate: atat in latime cat si pana la nivelul
;cel mai adanc. Daca gaseste o secventa care se aseamana cu patternul 
;dat va aplica patternul de output. In final va rezulta noul corp 
;al functiei care respecta regula data.

(define (replace-matching-seq inp outp body newbody)
  
  ;Functia care fapt face inlocuirea
  (define (replace-matching-seq-aux inp outp inbind body newbody)
    (if (null? body)
        newbody
        (if (list? body)
            (let ((head (car body)))
              (if (list? head)
                  ;Daca e lista se verifica daca nu cumva se potriveste 
                  ;cu patternul dat
                  (let  ((bind (matches-with-bindings inp head inbind)))
                    (if (equal? bind 'error)
                        ;Nu se potriveste
                        (let ((levelc (check-compatibility inp head 0)))
                          (if (>= levelc 0)
                              ;Insa exista "compatibilitate" pentru patternul dat (adica avem intr-o sublista
                              ;a liste respective patternul cauta
                              (append (bracket-app  (replace-matching-seq-aux inp outp inbind newbody '()) 
                                                    (replace-matching-seq-aux inp outp inbind head '())) 
                                      (replace-matching-seq-aux inp outp inbind (cdr body) '()))   
                              ;Patternul nu exista intr-o sublista
                              (replace-matching-seq-aux inp outp inbind (cdr body) (bracket-app newbody head))))
                        ;Se potriveste deci se inlocuieste cu patternul de output 
                        (let ((appliedp (apply-pattern outp bind '())))
                          ;Tratam un caz special de append
                          
                          (if (= (length appliedp) 1)
                              (replace-matching-seq-aux inp outp inbind (cdr body) (append newbody (apply-pattern outp bind '())))
                              (replace-matching-seq-aux inp outp inbind (cdr body) (bracket-app newbody (apply-pattern outp bind '())))))))
                  ;Daca nu e lista se trece pur si simplu mai departe (corpul functiei se mentine)
                  (replace-matching-seq-aux inp outp inbind (cdr body) (bracket-app newbody head))))
            body)))
  
  ;Functie auxiliara care face legarea variabilei ?this
  (define (match-with-this? inp outp body newbody)
    (replace-matching-seq-aux inp outp (list (list 'this? (cadr body))) body newbody))
  
  (match-with-this? inp outp body newbody))



(include "Reflection.scm")


;Returneaza true/false daca numele functiei 
;se gaseste in lista data ca parametru.

;Lista ar trebui sa fie de forma (+ (numef param) 1) sau 
;(if (zero? n) 
;    rez 
;    (numef (- n 1) (+ rez 1))
;In functie de ce contine o astfel de lista putem sa ne
;dam seama daca este recursivitate pe stiva (varianta 1)
;sau recursivitate pe coada (varianta 2)
(define (function-name-in-sublist? f L)
  (if (list? L)
      (if (null? L)
          false 
          (if (list? (car L))
              (if (equal? f (car (car L)))
                  true 
                  (function-name-in-sublist? f (cdr L)))
              (function-name-in-sublist? f (cdr L))))
      false))

;Functie de comparare a tipurilor de recursivitate 
;Am considerat ca recursivitatea pe stiva are prioritate. Daca 
;se intalneste acest tip de recursivitate se ignoara
;existenta recursivitatii pe coada.
(define recursion-comparator
  (lambda (x y) 
    (cond ((or (equal? x 'STACK-RECURSIVE) (equal? y 'STACK-RECURSIVE)) 'STACK-RECURSIVE)
          ((or (equal? x 'TAIL-RECURSIVE) (equal? y 'TAIL-RECURSIVE)) 'TAIL-RECURSIVE)
          (else 'NON-RECURSIVE))))

;Determina daca intr-un corp al functiei se gaseste o 
;secventa similara su exemplele date la function-name-in-sublist?.
;Si daca se gaseste se asigura daca primul termen din lista este if.
;Daca este => recursivitate pe coada. Daca pe prima pozitie a listei 
;de acest tip se gaseste orice altceva in afara de if inseamna ca 
;este recursivitate pe stiva.
(define (if-verify f L)
  (if (null? L)
      'NON-RECURSIVE  
      (if (list? (car L))
              (recursion-comparator (if-verify f (car L))
              (if (function-name-in-sublist? f (car L)) 
                  (cond ((equal? (car (car L)) 'if) 'TAIL-RECURSIVE)
                        (else 'STACK-RECURSIVE))
                  (if-verify f (cdr L))))
              (if-verify f (cdr L)))))


          
;Determina daca o functie care e scrisa cu clauze de tip cond 
;este cu recursivitate pe coada sau nu
;De obicei intr-o structura de tip cond ((...) (apel functie))
;nu se poate afla o structura recursiva pe stiva fara un set 
;auxiliar de paranteze. Iar daca acesta exista el va fi detectat 
;de functia anterior definita: if-verify.
(define (cond-verify f L)
  
  (define contains? 
    (lambda (e)
      (function-name-in-sublist? f e)))
      
  (if (null? L)
      'NON-RECURSIVE
      (let ((condexpresions (filter contains? L))) 
      (if (and (equal? (car L) 'cond) (not (null? condexpresions)))
          (if (list? (car condexpresions))
              'TAIL-RECURSIVE
              (if (or (list? (car L)) (equal? (car L) 'else))
                  (cond-verify f (car L))
                  (cond-verify f (cdr L))))
          (if (list? (car L))
              (recursion-comparator (cond-verify f (car L))
              (cond-verify f (cdr L)))
              (cond-verify f (cdr L)))))))


;Numarul numarul de aparatii al numelui 
;functiei in corpul acesteia
(define (count-occurrences f L n)
  ;Functie auxiliara care transforma o lista ce contine 
  ;liste in interiorul ei intr-o lista simpla
  (define (flatten-aux L Lrez)
  (if (null? L)
      Lrez
      (if (list? (car L))
          (flatten-aux (append (car L) (cdr L)) Lrez)
          (flatten-aux (cdr L) (append (cons (car L) '()) Lrez)))))
  ;Apeleaxa flatten-aux pentru a usura utilizarea
  (define (flatten L)
    (reverse (flatten-aux L '())))
  ;Se numara aparitiile 
  (let ((flattenedl (flatten L)))
    (if (null? flattenedl)
        n
        (if (equal? f (car L))
            (count-occurrences f (cdr flattenedl) (+ n 1))
            (count-occurrences f (cdr flattenedl) n)))))
        
  
  

;;Functia care detecteaza tipul recursivitatii
(define (detect-recursion f)
  (let* ((name (get-function-name f)) (body (get-function-lambda f)) 
         (res1 (if-verify name body)) (res2 (cond-verify name body)) (result (recursion-comparator res1 res2)))
    ;Se numara aparitiile pentru a vedea daca nu cumva avem recursiviate pe 
    ;pe arbore
    (if (and (equal? res1 'STACK-RECURSIVE) (not (equal? res2 'TAIL-RECURSIVE)))
        (if (> (count-occurrences name body 0) 1)
            'TREE-RECURSIVE
            result)
        result)))
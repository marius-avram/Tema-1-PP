;; Tema1 - PP
;; Mini - API pentru functii capabile de reflexie.


(define-syntax define-r
  (lambda (x)
    (syntax-case x ()
      ((_ (name . args) body)
       (syntax (define name
                 (make-reflection-function 'name (list 'lambda 'args 'body)))))
      ((_ name lambda-ex)
       (syntax (define name
                 (make-reflection-function 'name 'lambda-ex)))))))

(define (get-function-name-body f)
  (f '__REFLECTION__))

(define (get-function-name f) (car (get-function-name-body f)))

(define (get-function-lambda f) (cadr (get-function-name-body f)))

(define (get-function-body f) (caddr (get-function-lambda f)))

;;------------------------------------------------------------------------------

;; Functii ajutatoare (nu se vor folosi)

(define (make-reflection-function name lambda-ex)
  (if (not (valid-lambda-ex? lambda-ex))
      (error (format "INVALID LAMBDA EXPRESSION FOR FUNCTION ~a"
                            name))
      (let ((real-function (eval lambda-ex)))
        (lambda args
          (if (equal? args '(__REFLECTION__))
              (list name lambda-ex)
              (apply real-function args))))))


(define (valid-lambda-ex? ex)
  (and (list? ex)
       (has-length? 3 ex)
       (equal? 'lambda (car ex))
       (list? (cadr ex))))

(define (has-length? n L)
  (cond
    ((zero? n) (null? L))
    ((null? L) #f)
    (else (has-length? (sub1 n) (cdr L)))))


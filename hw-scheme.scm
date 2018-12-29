;;Question 1
(define poly-eval (lambda (ply x)
                      (if (null? ply)
                          '(0)
                          (if (null? (cdr ply))
                              (car ply)
                              (+ (car ply) (* x (poly-eval(cdr ply) x)))))))

;;Question 2
(define poly-add (lambda (ply1 ply2)
                   (cond ((null? ply1) ply2)
                         ((null? ply2) ply1)
                   (else
                    (cons (+ (car ply1) (car ply2))
                               (poly-add(cdr ply1) (cdr ply2)))))))

;;Question 3
(define poly-mul (lambda (ply1 ply2)
                   (if (null? ply2)
                       '(0)
                       (poly-add (poly-scalar ply1 (car ply2))
                                 (poly-mul (poly-degree ply1 1) (cdr ply2))))))

(define poly-scalar (lambda (ply x)
                      (if (null? ply)
                          '()
                          (cons (* x (car ply)) (poly-scalar (cdr ply) x)))))

(define poly-degree (lambda (ply n)
                      (append (poly-zero n) ply)))

(define poly-zero (lambda (n)
                     (if (< n 1)
                         '()
                         (cons 0 (poly-zero (- n 1))))))

;;Question 4
(define poly-diff (lambda (ply)
                    (cond((null? ply) '())
                         ((= (length ply) 1) '())
                    (else
                     (poly-diff-mul (cdr ply) 1)))))

(define poly-diff-mul (lambda (ply x)
                        (if (null? ply)
                            '()
                            (cons (* (car ply) x) (poly-diff-mul (cdr ply) (+ x 1))))))

;;Question 5
(define poly-int (lambda (ply)
                   (cond((null? ply) '())
                        ((= (length ply) 1) '())
                   (else
                    (cons 0 (poly-int-div ply 1))))))

(define poly-int-div (lambda (ply x)
                       (if (null? ply)
                           '()
                           (cons (/ (car ply) x) (poly-int-div (cdr ply) (+ x 1))))))

;;Question 6
(define grovel-poly-eval (lambda (s x)
                           (cond((null? s) '())
                           (else
                            (cond
                                 ((list? (car s)) (cons (poly-check (car s) x) (grovel-poly-eval (cdr s) x)))
                            (else
                             (cons (car s) (grovel-poly-eval (cdr s) x))))))))

(define poly-check (lambda (s x)
                     (cond((equal? (car s) 'poly) (poly-eval(cdr s) x))
                     (else
                      (cons (car s) (grovel-poly-eval (cdr s) x))))))
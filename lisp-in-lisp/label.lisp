
(defun label (nome e valori) 
	(lambda (cadr e) ;  lista degli argomenti
			(sost nome e) ; sostituisco nome il corpo con una nuova label
valori )
)

(defun sost (nome e)
	(cond ((null e) nil)
		  ((eq nome (car e)) (append `(label ,nome lambda ,(cadr e) ,(caddr e)) (sost nome (cdr e))))
		  ((cons (sost1 nome (car e)) (sost1 nome (cdr e))))))

(defun sost1 (nome e)
	(cond ((atom e) e)
		  (t (sost nome e))))



;(label 'fatt '(lambda (n) (if (= n 0) 1 (* n (fatt (- n 1))))) 8)
;(label 'somma '(lambda (x y) (+ x y)) 12 14)
;(label 'conta '(lambda (l) (if (null l) 0 (+ 1 (conta (rest l))))) '(a b a a a ))
;(label 'conta2 '(lambda (l1 l2) (if (null l1) (if (null l2) 0 (+ 1 (conta2 l1 (rest l2)))) (+ 1 (conta2 (rest l1) l2)))) '(a b c) '(a b c))




;(defun esegui_r (des sin)
;(setq sin (cons (car des) sin) )  
;(setq des (cdr des) )
;(cond ( (null des) (setq des '(BL)) ) )
;)

;(defun esegui_l (des sin)
;(cond ( (null sin) (setq sin '(BL)) ) )
;(setq des (cons (car sin) des) ) 
;(setq sin (cdr sin) )
;)


; mdt e' una lista di quadruple, ogni quadrupla e' una lista (q,s,x,q)

; macchina di turing per la somma
; (interprete 'q0 '(1 1 1 BL 1 1 1 1) nil '( (q0 1 BL q0) (q0 BL r q1) (q1 1 r q1) (q1 BL r q2) (q2 1 BL q2) ) ) 




(defun stampaid (sin stato des) 
	(print (list (reverse sin) stato des))
)

(defun creaclausola (quadrupla)

(cond   ( (eq (caddr quadrupla) 'R)  ; in questo caso esegui R
(list (list 'and (list 'eq 'stato (list 'quote (car quadrupla))) (list 'eq '(car des) (list 'quote (cadr quadrupla)) ) )
(list 'setq 'stato (list 'quote (car (cdddr quadrupla)))) 
 '(setq sin (cons (car des) sin) )
 '(setq des (cdr des) ) 
 '(cond ( (null des) (setq des '(BL)) )) 
)
)
 ;in questo caso esegui L 
( (eq (caddr quadrupla) 'L) 
(list (list 'and (list 'eq 'stato (list 'quote (car quadrupla))) (list 'eq '(car des) (list 'quote (cadr quadrupla)) ) )
(list 'setq 'stato (list 'quote (car (cdddr quadrupla)))) 
'(cond ( (null sin) (setq sin '(BL)) )) 
'(setq des (cons (car sin) des) ) 
'(setq sin (cdr sin) )
)
)
;se la quadrupla non contiene r o l contiene un simbolo
(T (list (list 'and (list 'eq 'stato (list 'quote (car quadrupla))) (list 'eq '(car des) (list 'quote (cadr quadrupla)) ) )
(list 'setq 'stato (list 'quote (car (cdddr quadrupla))))
(list 'setq 'des (list 'cons (list 'quote (caddr quadrupla)) '(cdr des) ) )
)
)

) ;fine dei 3 casi
)


(defun creacorpo (mdt)
	(cond ( (null mdt) nil)
		(T (append (list (creaclausola (car mdt))) (creacorpo (cdr mdt)))
)) ; fine del cond
) ; fine di creacorpo

(defun compilatore (nomedimdt mdt)
	(list 'defun nomedimdt '(sin stato des)
		(list 'do '() '(nil nil) 
			'(stampaid sin stato des)
			(append '(cond)
			(creacorpo mdt)
			(list '(t (return '(fine del calcolo))))
			)
		)
	)
)

(defun compila (nomedimdt mdt)
	(eval (compilatore nomedimdt mdt))
)
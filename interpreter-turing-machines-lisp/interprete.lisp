; esempio macchina di turing per la somma
; (interprete '(() q0 (1 1 1 BL 1 1 1 1)) '( (q0 1 BL q0) (q0 BL r q1) (q1 1 r q1) (q1 BL r q2) (q2 1 BL q2) ) ) 

; mdt e' una lista di quadruple, ogni quadrupla e' una lista (q,s,x,q)
; ID = (inverso della lista sinistra Stato Q Lista destra)
; sin = (sm...s1)
; des = (s2....sn)
; sin = (car id)
; stato = (cadr id)
; des =(caddr id)
; simbolo letto = (car des) = (caaddr id)


(defun stampaid (id) 
(print (list (reverse (car id)) (cadr id) (caddr id)))
)

(defun trova (mdt stato simbolo)
(cond 
((null mdt) nil) ; fine ricorsione
		((and (eq (car (car mdt)) stato)
			(eq (cadr (car mdt)) simbolo)) ; se inizia per qs voluti
				(car mdt)) ; restituisce quadrupla
		(T (trova (cdr mdt) stato simbolo)) ;ricorri su cdr mdt
	)
)

(defun interprete (id mdt)
	(stampaid id) ; stampa la descrizione istantanea

; cerca una quadrupla da eseguire ed eseguila
	(setq quad (trova mdt (cadr id) (caaddr id)))
	(cond ( (null quad)   '(fine del calcolo) );finito
	(T 
(interprete 	(cond   
		( (eq (caddr quad) 'R) ; in questo caso esegui R
			(list (cond ( (null (caddr id)) (cons 'BL (car id)) ) ( T (cons (caaddr id) (car id))) );questo crea la lista di sinistra dopo un R
				(cadddr quad) ; il nuovo stato
				(cond ( (null (cdaddr id)) '(BL) ) (T  (cdaddr id)) ); la nuova lista destra
			)) ; finito di eseguire R

		( (eq (caddr quad) 'L)  ;in questo caso esegui L 
			(list (cdar id) ; la nuova lista sinistra
				(cadddr quad) ; il nuovo stato
				(cond ( (null (caar id)) (cons 'BL (caddr id)) ) ( T (cons (caar id) (caddr id))) );questo crea la lista di destra dopo un L
			)) ; finito di eseguire R
		( T (list (car id) (cadddr quad) (cons (caddr quad) (cdaddr id)) ) ) ;rimpiazzo il simbolo sul nastro
	) ;fine della nuova ID
	mdt ; la mdt e' la stessa
) ; fine della chiamata ad interprete
	) )
)

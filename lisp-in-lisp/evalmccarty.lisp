(label 'evalm 
	'(lambda (e a)
		(cond 
		  ((atom e) 
				(cond ((eq e nil) nil)
					  ((eq e t) t) 
					   (t (cdr  (label 'assoc '(lambda (e a) (cond ((null a) nil) 
											                       ((eq e (caar a)) (car a))
											                       (t (assoc e (cdr a)))    
											                      
											                );;fine cond
								             );; fine lambda
								       e
								       a
								 );;fine label
							    
						   );;fine cdr
					   );;fine t
				);;fine cond
		  );;fine (atom e)	
		((atom (car e))
			(cond
			 ((eq (car e) (quote quote)) (cadr e) )
			 ((eq (car e) (quote car))   (car (evalm (cadr e) a)) )
			 ((eq (car e) (quote cdr))   (cdr (evalm (cadr e) a)) )
			 ((eq (car e) (quote cadr)) (cadr (evalm (cadr e) a)) )
			 ((eq (car e) (quote caddr)) (caddr (evalm (cadr e) a)) )
			 ((eq (car e) (quote caar)) (caar (evalm (cadr e) a)) )
			 ((eq (car e) (quote cadar)) (cadar (evalm (cadr e) a)) )
			 ((eq (car e) (quote caddar)) (caddar (evalm (cadr e) a)) )
			 ((eq (car e) (quote atom)) (atom (evalm (cadr e) a)) )
			 ((eq (car e) (quote null)) (null (evalm (cadr e) a)) )
			 ((eq (car e) (quote cons)) (cons (evalm (cadr e) a) (evalm (caddr e) a)) )
			 ((eq (car e) (quote eq)) (eq (evalm (cadr e) a) (evalm (caddr e) a)) )
			 ((eq (car e) (quote cond))  
			
						 (label 'evcond '(lambda (v a) (cond ((evalm (caar v) a) 
										                    (evalm (cadar v) a)
										                   )  
									                       (t (evcond (cdr v) a)
										                   )
									                 )
								
								       );; lambda
							           (cdr e)
							           a
							
						 );; fine label			
			 );;*****
			 (t (evalm (cons
					     (cdr (label 'assoc '(lambda (e a)
											(cond ((null a) nil)
												  ((eq e (caar a)) (car a))
												  (t (assoc e (cdr a)))
											);; fine cond
										  );; fine lambda  
							  (car e)
							  a
							  );; label	
						  );; cdr
						
						(cdr e)				
					   );; fine cons
				a
             );; fine evalm
						
			 );; fine true
			);; fine cond
		);; fine (atom (car e))
		
			((eq (caar e) (quote lambda))
					  (evalm (caddar e) '(label 'ffappend '(lambda (u v) (cond ((null u) v)
										                                    (t (cons (car u) (ffappend (cdr u) v ))) 
										                              ) 
									                    )
								               '(label 'pairup '(lambda (u v)
										                     (cond ((null u) nil)
										                                    (t (cons (cons (car u) (car v)) 
													                           (pairup (cdr u) (cdr v) ))) 
										                              )
										                     )
									                  (cadar e)
									                  '(label 'evlis '(lambda (u a)
											                       (cond ((null u) nil)
										                                    (t (cons (evalm (car u) a) (evlis (cdr u) a ))) 
										                              )
											                       )
										              (cdr e)
										              a
										              );; fine
									             
									           );; fine argomenti label
								          a    
								        );; fine label
							
					  );; fine evalm
		);; fine (eq (caar e) (quote lambda))
			 	
	 ((eq (caar e) (quote label)) 
				    (evalm (cons (caddar e) (cdr e)) (cons (cons (cadar e) (car e)) a))
	 );; fine eq	
				
   );; cond iniziale
     
	);; fine lambda
	 '((lambda (x) (car x)) '(a b c))
	 '()
) ;;fine label





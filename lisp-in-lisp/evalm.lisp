(defun EVALM (espressione ambiente) ((LAMBDA (E A)
   (COND
    ((ATOM E)
(COND 
((EQ E NIL) NIL) 
((EQ E T) T)
(T   (CaDR
        (LABEL 'ASSOC
         '(LAMBDA (E A)
           (COND ((NULL A) NIL) ((EQ E (CAAR A)) (CAR A))
            (T (ASSOC E (CDR A)))))
         E A))))) ; fine della condizione atom E
    ((ATOM (CAR E))
     (COND ((EQ (CAR E) 'QUOTE) (CADR E))
      ((EQ (CAR E) 'CAR) (CAR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CDR) (CDR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CADR) (CADR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CADDR) (CADDR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CAAR) (CAAR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CADAR) (CADAR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CADDAR) (CADDAR (EVALM (CADR E) A)))
      ((EQ (CAR E) 'ATOM) (ATOM (EVALM (CADR E) A)))
      ((EQ (CAR E) 'NULL) (NULL (EVALM (CADR E) A)))
      ((EQ (CAR E) 'CONS) (CONS (EVALM (CADR E) A) (EVALM (CADDR E) A)))
      ((EQ (CAR E) 'EQ) (EQ (EVALM (CADR E) A) (EVALM (CADDR E) A)))
      ((EQ (CAR E) 'COND)
       (LABEL 'EVCOND
        '(LAMBDA (V A)
          (COND ((EVALM (CAAR V) A) (EVALM (CADAR V) A))
           (T (EVCOND (CDR V) A))))
        (CDR E) A))
      (T
       (EVALM
        (CONS
         (CaDR
          (LABEL 'ASSOC
           '(LAMBDA (E A)
             (COND ((NULL A) NIL) ((EQ E (CAAR A)) (CAR A))
              (T (ASSOC E (CDR A)))))
           (CAR E) A))
         (CDR E))
        A)) ; altrimenti, se l'atomo E non e' una funzione nota, cercala nell'ambiente A e rivaluta

)) ; finite le azioni nel caso E atomo.

((EQ (CAAR E) 'LAMBDA) ; se E e' una lambda espressione
     (EVALM (CADDAR E)
      (LABEL 'FFAPPEND
        '(LAMBDA (U V)
          (COND ((NULL U) V) (T (CONS (CAR U) (FFAPPEND (CDR U) V)))))
        (LABEL 'PAIRUP
          '(LAMBDA (U V)
            (COND ((NULL U) NIL)
             (T (CONS (CONS (CAR U) (CAR V)) (PAIRUP (CDR U) (CDR V))))))
          (CADAR E)
          (LABEL 'EVLIS
            '(LAMBDA (U A)
              (COND ((NULL U) NIL)
               (T (CONS (EVALM (CAR U) A) (EVLIS (CDR U) A)))))
            (CDR E) A))
        A))) ; fine caso di E lambda espressione

    ((EQ (CAAR E) 'LABEL) ; se E e' una label
     (EVALM (CONS (CADDAR E) (CDR E)) (CONS (CONS (CADAR E) (CAR E)) A))) ;fine del caso E label

) ;fine del cond toplevel
) ;fine della lambda espressione

espressione ambiente)

)


#! /usr/bin/env gsi -:dR

;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la première section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; être définie, et vous pouvez ajouter des définitions de fonction
;;; afin de bien décomposer le traitement à faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter reçoit en paramètre une liste de caractères
;;; contenant la requête lue et le dictionnaire des variables sous
;;; forme d'une liste d'association.  La fonction retourne
;;; une paire contenant la liste de caractères qui sera imprimée comme
;;; résultat de l'expression entrée et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "repl" qui se charge de cela.

(define is-letter?
	(lambda (str)
	(if (= 1 (string-length str))
	 (if (char-alphabetic? (string-ref str 0))
		#t
		#f)
		#f)))


(define foldr ;;; parce que scheme c'est un language hipster pis faudrait surtout pas définir les trucs hyper communs
	(lambda (f base lst)
	(if (null? lst)
	base
	(f (car lst)
	(foldr f base (cdr lst))))))

(define push-stack
	(lambda (item stk)
		(if (null? stk )
		(cons item '())
		(cons item stk))))

(define peek-stack
	(lambda (stk)
		(if (null? stk)
		(error "empty stack" stk)
		(car stk))))

(define pop-stack
	(lambda (stk)
		(if(null? stk)
		(error "empty stack" stk)
		(cdr stk))))
		
(define len-stack ;;; lol
	(lambda (stk)
		(length stk)))

(define string-mult
	(lambda (s1 s2)
	(* (string->number s1) (string->number s2))))
	
(define string-add 
	(lambda (s1 s2)
	(+ (string->number s1) (string->number s2))))
	
(define string-sub 
	(lambda (s1 s2)
	(- (string->number s1) (string->number s2))))

(define superfonction
  (lambda (str)
  (transformer (split (str->list str) char-whitespace?))))
  
(define transformer
 (lambda (grosse-liste)
  (map list->string grosse-liste)))

(define split
  (lambda (lst pred)
  (let chelou ((fini '()) 
               (restant lst)
               (enCours '()))
   (if (null? restant)
    (if (null? enCours)
     fini
     (reverse (cons (reverse enCours) fini)))
   (if (pred (car restant))
   (chelou ( if (null? enCours)
      fini
      (cons (reverse enCours) fini))
      (cdr restant)
      '())
    (chelou 
      fini
      (cdr restant)
      (cons (car restant) enCours)))))))
    
(define traiter
  (lambda (expr dict)
    (cons (append (string->list "*** le programme est ")
                  '(#\I #\N #\C #\O #\M #\P #\L #\E #\T #\! #\newline)
                  (string->list "*** la requete lue est: ")
                  expr
                  (string->list "\n*** nombre de caractères: ")
                  (string->list (number->string (length expr)))
                  '(#\newline))
          dict)))

;;;----------------------------------------------------------------------------

;;; Ne pas modifier cette section.

(define repl
  (lambda (dict)
    (print "? ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (let ((r (traiter-ligne ligne dict)))
            (for-each write-char (car r))
            (repl (cdr r)))))))

(define traiter-ligne
  (lambda (ligne dict)
    (traiter (string->list ligne) dict)))

(define main
  (lambda ()
    (repl '()))) ;; dictionnaire initial est vide
    
;;;----------------------------------------------------------------------------

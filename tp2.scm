#! /usr/bin/env gsi -:dR

;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la premi�re section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; �tre d�finie, et vous pouvez ajouter des d�finitions de fonction
;;; afin de bien d�composer le traitement � faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter re�oit en param�tre une liste de caract�res
;;; contenant la requ�te lue et le dictionnaire des variables sous
;;; forme d'une liste d'association.  La fonction retourne
;;; une paire contenant la liste de caract�res qui sera imprim�e comme
;;; r�sultat de l'expression entr�e et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "repl" qui se charge de cela.

(define superfonction
  (lambda (str)
  (transformer (split str char-whitespace?))))
  
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
                  (string->list "\n*** nombre de caract�res: ")
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

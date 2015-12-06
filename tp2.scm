#! /usr/bin/env gsi -:dR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IFT2035 - Travail pratique 2                      ;;
;; Calculatrice à précision infinie                  ;;
;;                                                   ;;
;; Sulliman Aïad <sulliman.aiad@umontreal.ca>        ;;
;; François Poitras <francois.poitras@umontreal.ca>  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tout doit être un nombre, un opérateur, une variable [a-z], ou un opérateur de variable assorti à une lettre [a-z].
(define accepted_operators '(#\+ #\- #\*))
(define variable_operator #\=)
      
; http://computer-programming-forum.com/40-scheme/089302221d8fd75e.htm
(define implode
  (lambda (s*)
    (string->symbol
      (apply string-append
             (map symbol->string s*))))) 

; http://stackoverflow.com/a/5007129/5354535
(define (remove-last lst)
  (if (null? (cdr lst))
    '()
    (cons (car lst) (remove-last (cdr lst)))))

; Sépare une chaîne de caractères en liste d'éléments en scindant à un caractère particulier.
(define split
  (lambda (lst pred)
    (let struct ((fini '()) 
                 (restant lst)
                 (en-cours '()))
    
      (if (null? restant)
        (if (null? en-cours)
          fini
          
          (reverse (cons (reverse en-cours) fini)))
          
        (if (pred (car restant))
          (struct (if (null? en-cours)
                    fini
                    (cons (reverse en-cours) fini))
                  (cdr restant)
                  '())
          
          (struct fini
                  (cdr restant)
                  (cons (car restant) en-cours)))))))

; Permet de retirer une variable du dictionnaire.
; Inspiré par : http://stackoverflow.com/a/4452203/5354535
(define delete-var
  (lambda (var dict)
    (let ((couple (assoc var dict)))
      (cond
        ((null? dict) dict)
        ((eq? couple (car dict)) (delete-var var (cdr dict)))
        (else (cons (car dict) (delete-var var (cdr dict))))))))

; Permet d'affecter une valeur à une variable dans le dictionnaire (ajout ou modification).
(define affect-var
  (lambda (dict var value)
    (if (assoc var dict)
      ; La variable existe déjà, on reconstruit sans, et on rajoute.
      (affect-var (delete-var var dict) var value)
      
      ; La variable n'existe pas encore, on rajoute.
      (append dict (list (list var value))))))

; Géante fonction récursive qui traite tous les cas de figure et retourne la réponse à une commande.
(define process
  (lambda (input stack dict output?)
    (let ((return (if (= (length input) 0)
      ; S'il n'y a plus rien comme commandes.
      (if (= (length stack) 0)
        ; Et si le stack est vide, retourner une erreur.
        (cons (string->list "Veuillez entrer une commande.") dict)
        
        (if (= (length stack) 1)
          ; Sinon, si le stack ne contient que le résultat final.
          (cons (string->list (number->string (car (reverse stack)))) dict)
          
          ; Si le stack contient trop d'éléments (> 1).
          (cons (string->list "Erreur de syntaxe: veuillez revoir votre commande.") dict)))
      
      (if (string->number (list->string (car input)))
        ; S'il s'agit d'un nombre.
        ; Enregistrer le nombre dans le stack.
        (process (cdr input) (append stack (cons (string->number (list->string (car input))) '())) dict #f)
        
        (if (member (caar input) accepted_operators)
          ; Sinon, s'il s'agit d'un opérateur.
          ; Exécuter l'opération si possible sur les deux derniers nombres du stack.
          
          (if (>= (length stack) 2)
            ; S'il y a au moins deux nombres dans le stack.
            (let ((result ((if (eq? (caar input) #\+) +
                           (if (eq? (caar input) #\-) -
                           (if (eq? (caar input) #\*) *)))
                          (cadr (reverse stack)) (car (reverse stack)))))
                          
                 (process (cdr input)
                               (append (remove-last (remove-last stack)) ; Pour retirer les deux derniers éléments du stack.
                                       (cons result '())) ; Et ensuite rajouter le résultat calculé.
                               dict
                               #f))
            
            ; Sinon, il manque un élément pour faire une opération.
            (cons (string->list "Commande invalide, erreur de syntaxe.") dict))
          
          (if (and (eq? (caar input) variable_operator) (= (length (cdar input)) 1) (char<=? #\a (cadar input)) (char>=? #\z (cadar input)))
            ; Sinon, si le premier caractère est un opérateur de variable ("=") et que le deuxième caractère est une lettre entre [a-z].
            (if (> (length stack) 0)
              ; S'il y a une valeur dans le stack, on l'affecte, et on passe à la prochaine commande (s'il y en a) sans toucher au stack.
              (process (cdr input) stack (affect-var dict (cadar input) (car (reverse stack))) #f)
              
              ; S'il n'y a pas de valeur dans le stack à affecter.
              (cons (string->list "Quelle valeur voulez-vous affecter dans cette commande ?") dict))
            
            (if (and (= (length (car input)) 1) (char<=? #\a (caar input)) (char>=? #\z (caar input)))
              ; Sinon, s'il s'agit d'une variable, ajouter son contenu au stack.
              (if (assoc (caar input) dict)
                ; Si la variable est affectée, on la met dans le stack et on continue.
                  (process (cdr input)
                           (append stack (cdr (assoc (caar input) dict)))
                           dict
                           #f)
                
                  ; Si la variable n'est pas affectée.
                  (cons (string->list "Aucune affectation faite pour cette variable.") dict))
              
              ; Si ce n'est rien de tout ça, arrêter tout traitement et retourner une erreur.
              (cons (string->list "Commande invalide, erreur de syntaxe.") dict))))))))
    
      (if output?
        ; Appel final.
        (cons (append (car return) '(#\newline)) (cdr return))
        
        ; Appel interne.
        return))))

; La fonction traiter fait appel à "process" en commençant avec un stack vide.
(define traiter
  (lambda (expr dict)
    (process (split expr char-whitespace?) '() dict #t)))

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


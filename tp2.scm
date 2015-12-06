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
                 (enCours '()))
    
        (if (null? restant)
          (if (null? enCours)
            fini
            
            (reverse (cons (reverse enCours) fini)))
            
          (if (pred (car restant))
            (struct (if (null? enCours)
                      fini
                      (cons (reverse enCours) fini))
                    (cdr restant)
                    '())
            
            (struct fini
                    (cdr restant)
                    (cons (car restant) enCours)))))))

; Géante fonction récursive qui traite tous les cas de figure et retourne la réponse à une commande.
(define process
  (lambda (input stack)
    (if (= (length input) 0)
      ; S'il n'y a plus rien comme commandes.
      (if (= (length stack) 0)
        ; Et si le stack est vide, retourner une erreur.
        (string->list "Veuillez entrer une commande.")
        
        (if (= (length stack) 1)
          ; Sinon, si le stack ne contient que le résultat final.
          (string->list (number->string (car (reverse stack))))
          
          ; Si le stack contient trop d'éléments (> 1).
          (string->list "Erreur de syntaxe: veuillez revoir votre commande.")
        )
      )
      
      (if (string->number (list->string (car input)))
        ; S'il s'agit d'un nombre.
        ; Enregistrer le nombre dans le stack.
        (process (cdr input) (append stack (cons (string->number (list->string (car input))) '())))
        
        (if (member (caar input) accepted_operators)
          ; Sinon, s'il s'agit d'un opérateur.
          ; Exécuter l'opération si possible sur les deux derniers nombres du stack.
          
          (if (>= (length stack) 2)
            ; S'il y a au moins deux nombres dans le stack.
            (let ([result ((if (eq? (caar input) #\+) +
                           (if (eq? (caar input) #\-) -
                           (if (eq? (caar input) #\*) *)))
                          (cadr (reverse stack)) (car (reverse stack)))])
                          
                 (process (cdr input)
                          (append (remove-last (remove-last stack)) ; Pour retirer les deux derniers éléments du stack.
                                  (cons result '()))) ; Et ensuite rajouter le résultat calculé.
            )
            
            ; Sinon, il manque un élément pour faire une opération.
            (string->list "Commande invalide, erreur de syntaxe.")
          )
          
          (if (eq? (caar input) variable_operator)
            ; Sinon, si le premier caractère est un opérateur de variable ("=") et que ***TODO : le deuxième caractère est une lettre entre [a-z]***
            ; Enregistrer le top de la pile dans la variable correspondante (CDAR)... TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            (car (reverse stack))
            
            (if (eq? (caar input) "TODO")
              ; Sinon, s'il s'agit d'une variable, ajouter son contenu au stack.
              (string->list "TODOOODODOOOOOOOOO")
              
              ; Si ce n'est rien de tout ça, arrêter tout traitement et retourner une erreur.
              (string->list "Commande invalide, erreur de syntaxe.")
            )
          )
        )
      )
    )
  )
)

; La fonction traiter fait appel à "process" en commençant avec un stack vide.
(define traiter
  (lambda (expr dict)
    (cons (append (process (split expr char-whitespace?) '()) '(#\newline)) dict)
  )
)

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


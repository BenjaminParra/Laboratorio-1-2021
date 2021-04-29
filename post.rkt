#lang racket
(require "tdaUsuario.rkt")
;TDA POST
;date es la fecha de creacion de la cuenta


;(post socialNetwork) asi debe ser


#| Funcion que se encarga de tranforma una fila a string
 Entrada: fila x string
 Salida: String|#
(define(listToString lista str)
  (if (null? lista)
      str
      (if(string? (car lista))
         (listToString (cdr lista)(string-append str " " (car lista)))
         (listToString (cdr lista)(string-append str " "(number->string(car lista)))))
      )
  )

#|(define (post socialNetwork)
  (lambda (getUser)(lambda getDate)))
 )|#
;(display (creaPublicacion "BenjaParra" (list 1 3 24) "Estoy muy feliz" "estado" "chilo"))

;descripción: Función que verifica si usuario está en la lista de amigos
;dom: string X lista
;rec: boolean
(define (sonAmigos? user listaAmigo)
  (if (null? listaAmigo)
      #f
      (if (equal? user (car listaAmigo))
          #t
          (sonAmigos? user (cdr listaAmigo)))))

;funcion que recibe UsuarioCreador Date Archivo Tipo $destino opcional
(define (creaPublicacion . args)
  (if (= 4 (length args))
      (string-append "El usuario " (car args) " ha publicado " (caddr args) "."
                     (cadddr args) " en su perfil." "\n" "Fecha:" (listToString(cadr args)""))
      (if (sonAmigos? (car(cdr (cdddr args))) (list "chilo" "benja" "parra")) ;modificar aca y colocar lista de amigos
           (if (= 5 (length args))
               (string-append "El usuario " (car args) " ha publicado "  (caddr args) "."
                         (cadddr args) " en el perfil de " (car(cdr (cdddr args))) " \n" "Fecha:" (listToString(cadr args)""))
               #f)
           #f)
      )
  )

  



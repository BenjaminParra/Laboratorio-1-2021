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


;descripción: Función que verifica si usuario está en la lista de amigos
;dom: string X lista
;rec: boolean
(define (sonAmigos? user listaAmigo)
  (if (null? listaAmigo)
      #f
      (if (equal? user (car listaAmigo))
          #t
          (sonAmigos? user (cdr listaAmigo)))))


(define (listaPost . lista)lista)
;(display (creaPublicacion "BenjaParra" (list 1 3 24) "Estoy muy feliz" "estado" "chilo"))
;funcion que recibe lista con UsuarioCreador Date Archivo Tipo $destinoOpcional
(define (creaPublicacion parametrosPost)
  (if (= 4 (length parametrosPost))
      (string-append "El usuario " (car parametrosPost) " ha publicado " (caddr parametrosPost) "."
                     (cadddr parametrosPost) " en su perfil." "\n" "Fecha:" (listToString(cadr parametrosPost)""))
      (if (sonAmigos? (car(cdr (cdddr parametrosPost))) (list "chilo" "benja" "parra")) ;modificar aca y colocar lista de amigos
           (if (= 5 (length parametrosPost))
               (string-append "El usuario " (car parametrosPost) " ha publicado "  (caddr parametrosPost) "."
                         (cadddr parametrosPost) " en el perfil de " (car(cdr (cdddr parametrosPost))) " \n" "Fecha:" (listToString(cadr parametrosPost)""))
               #f)
           #f)
      )
  )

;Funcion Creadora post
;User1 date contenido tipo user 2(opcional)
(define (creaPost . elementos)elementos)

#|SELECTORES|#
;descripción: Función que retorna el usuario que realiza el post
;dom: 
;rec: entero
  
;(define (suma x y)(+ x y))

;(define (operacion x y resultado)
 ; (- resultado (suma x y)))


;(define (post socialnetwork)(lambda (date)(lambda (content))))

(define prueba (lambda (a)(lambda (content . b)
                          b)))
;(define prueba (lambda (que . user) user))
(define post (lambda (socialnetwork)
  (lambda (date) (lambda (contenido . usuarios)usuarios))))

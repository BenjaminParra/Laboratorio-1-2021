#lang racket
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "Date.rkt")

(provide (all-defined-out))
;(define(social->string socialnetwork))
;(socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn" listaUser listaPost)
;user pass amigos perfil date estado
;(cadddr post)

(define(traduceUser user)
  (cond
    [(and (null? (getListaPostUser user))(null? (getAmigos user)))(string-append "El Usuario " (getUser user) " no tiene amigos y no ha realizado alguna publicacion")]
    [(and (null? (getListaPostUser user))(not(null? (getAmigos user))))(string-append "El Usuario " (getUser user)
                                            " sus amigos son "
                    (traduceAmigos (getListaNames(getAmigos user))) ", su perfil no contiene ninguna publicacion ")]
    [(and (not(null? (getListaPostUser user)))(null? (getAmigos user)))(string-append "El Usuario " (getUser user)
                                            " no tiene amigos registrados, su perfil " (traduceListaPost (getListaPostUser user)))]
    
    [(string-append "El usuario " (getUser user) " sus amigos son "
                    (traduceAmigos (getListaNames(getAmigos user))) " su perfil es "(traduceListaPost (getListaPostUser user)) "se encuentra "
                                                      (getEstado user))]))
#|(display (traduceUser'("benja"
  "123"
  (("chilo" "123" () () (1 2 2021) "offline") ("juli" "123" () () (1 2 2021) "offline"))
  ()
  (1 2 2021)
  "offline")))|#
(define(traduceListaUser listauser)
  (if (null? listauser)
      ""
      (string-append(string-append (traduceUser (car listauser))"\n")(traduceListaUser (cdr listauser)))))
#|(display (traduceListaUser '(("benja"
   "123"
   (("chilo" "123" () () (1 2 2021) "offline") ("juli" "123" () () (1 2 2021) "offline"))
   ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ()))
   (1 2 2021)
   "offline")
  ("chilo" "123" () () (1 2 2021) "offline")
  ("juli" "123" () () (1 2 2021) "offline"))))|#
(define(traduceAmigos amigosUser)
 (if(null? amigosUser)
    ""
    (string-append (string-append(car amigosUser) "\n")(traduceAmigos (cdr amigosUser)))))

(define(traducePost post);post trae tda user, 
  
  (if (null? (cadddr post))
      (string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                  "." "\n" "Fecha:" (listToString(cadr post)""))
      (string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                 " en el perfil de " (getUser(car(cadddr post))) "." "\n" "Fecha:" (listToString(cadr post)"")))
  )
(define(traduceListaPost listaPost)
  (if (null? listaPost)
      ""
      (string-append (string-append(traducePost (car listaPost))"\n")(traduceListaPost (cdr listaPost)))))


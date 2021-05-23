#lang racket
(provide (all-defined-out))
;(require "post.rkt")
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "Date.rkt")

#|

(define(follow socialnetwork)
  (lambda (date)(lambda (user2)
    (if(and(socialnetwork? socialnetwork)
           (date? date)
           (registradoSN? socialnetwork user2))
            (if (not(sonAmigosFollow? user2 (getAmigos(getUserOnlineSN socialnetwork))))
                (turnOffFollow(aplicaSetUserPost socialnetwork (getUserOnlineSN socialnetwork)
                                   setListaAmigos (getTdaUser socialnetwork user2)))
                
                 (turnOffFollow socialnetwork))
            #f) 
       )))|#
#|
;Funcion que luego se aplicar una funcion ya sea post o lo que sea se vuelve Offline
(define(turnOffFollow sn)
  (if (socialnetwork? sn)
      (setListaUser sn (aplicaTurnOff (getListaUser sn)));(setUser sn (getUserOnlineSN sn) setEstado "offline" )))
      #f))

(define(aplicaTurnOff  listaUser)
  
      (if (null? listaUser)
          listaUser
          (if (eqv? (getEstado(car listaUser))"online")
              (cons (setEstado (car listaUser) "offline")(cdr listaUser))
              (cons (car listaUser)(aplicaTurnOff  (cdr listaUser)));ACAAA
      )
      )
  )|#

#|
((aplicaPublicaUser sn (car(getListaUser '("facebook"
  (25 10 2021)
  "encryptFn"
  "encryptFn"
  (("benja" "123" () ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ())) (1 2 2021) "online")
   ("chilo" "123" () () (1 2 2021) "offline"))
  ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ()))))) '("chilo" "juli")) '(25 10 2021) "textoParaOtro.text")
;descripción: Función que verifica si usuario está en la lista de amigos
;dom: string X lista
;rec: boolean
(define (sonAmigosFollow? user listaAmigo)
  (if (null? listaAmigo)
      #f
      (if (equal? user (getUser(car listaAmigo)))
          #t
          (sonAmigosFollow? user (cdr listaAmigo)))))|#
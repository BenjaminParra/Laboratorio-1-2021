#lang racket
;(require "post.rkt")
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "Date.rkt")
(provide (all-defined-out))

(provide (all-defined-out))
(define(follow socialnetwork)
  (lambda (date)(lambda (user2)
    (if(and(socialnetwork? socialnetwork)
           (date? date)
           (registradoSN? socialnetwork user2))
            (if (not(sonAmigosFollow? user2 (getAmigos(getUserOnlineSN socialnetwork))))
                (turnOff(aplicaSetUserPost socialnetwork (getUserOnlineSN socialnetwork)
                                   setListaAmigos (getTdaUser socialnetwork user2)))
                (turnOff socialnetwork))
            #f)
       )))

;descripción: Función que verifica si usuario está en la lista de amigos
;dom: string X lista
;rec: boolean
(define (sonAmigosFollow? user listaAmigo)
  (if (null? listaAmigo)
      #f
      (if (equal? user (getUser(car listaAmigo)))
          #t
          (sonAmigosFollow? user (cdr listaAmigo)))))
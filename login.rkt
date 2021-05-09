#lang racket
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "Date.rkt")
(provide (all-defined-out))
;(login socialnetwork username password operation)
#|
(define(login socialnetwork username password operation)
  (cond
   [(not(socialnetwork? sn)) (error "Ingrese un socialnetwork valido")]
   [(not(date? date)) (error "Ingrese una fecha valida")]
   [(not(string? username))(error "Ingrese username que sea string")]
   [(not(string? password)) (error "Ingrese password que sea string")]
   [(not(password? password username)) (error "Ingrese password distinto al username, de largo minimo 6 que contengas numero y letras")]
   [(not(>= (string-length username)4))(error "Ingrese un Username de minimo 4 caracteres")]
   [(contieneSpace (string->list username))(error "Ingrese un Username sin espacios")]
   [(contieneSpace (string->list password))(error "Ingrese un password sin espacios")]
   [(not(registradoSN? socialnetwork username))(error "El usuario no esta registrado")]
   []))|#

(define(verificaLogin user pass listaUser)
  (if(null? listaUser)
     (display "Usuario no encontrado")
     (if (eqv? user (car (getListaNames listaUser)))
         (if (eqv? pass (getPassword (car listaUser)))
             (setEstado (car listaUser) "online")
             (display "Clave incorrecta"))
     (verificaLogin user pass (cdr listaUser)))))


#lang racket
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
;(require "Date.rkt")
;(require "post.rkt")
(provide verificaLogin)
;(provide (all-defined-out))
;(login socialnetwork username password operation)
#|
(define(login socialnetwork username password operation)(lambda (date)(lambda (param1 . param2)
  (cond
   [(not(socialnetwork? socialnetwork)) (error "Ingrese un socialnetwork valido")]
   ;[(not(date? date)) (error "Ingrese una fecha valida")]
   [(not(string? username))(error "Ingrese username que sea string")]
   [(not(string? password)) (error "Ingrese password que sea string")]
   ;[(not(password? password username)) (error "Ingrese password distinto al username, de largo minimo 6 que contengas numero y letras")]
   [(not(>= (string-length username)4))(error "Ingrese un Username de minimo 4 caracteres")]
   ;[(contieneSpace (string->list username))(error "Ingrese un Username sin espacios")]
   ;[(contieneSpace (string->list password))(error "Ingrese un password sin espacios")]
   [(not(registradoSN? socialnetwork username))(error "El usuario no esta registrado")]
   [(verificaLogin username password (getListaUser socialnetwork))
    (if (eqv? operation "socialnetwork->string")
        (operation (aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))
        (((operation (aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))date)param1 param2))]))))|#
;LUEGO DE TERMINAR UNA FUNCION DEBE COLOCAR ON EN EL USUARIO
(define(verificaLogin user pass listaUser)
  (if(null? listaUser)
     #f;(display "Usuario no encontrado")
     (if (eqv? user (car (getListaNames listaUser)))
         (if (eqv? pass (getPassword (car listaUser)))
             #t;(setEstado (car listaUser) "online")
             #f);(display "Clave incorrecta"))
     (verificaLogin user pass (cdr listaUser)))))


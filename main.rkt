#lang racket
(require "post.rkt")
(require "socialNetwork.rkt")
(require "tdaUsuario.rkt")
(require "share.rkt")
(require "date.rkt")
(require "register.rkt")
(require "login.rkt")
(require "socialnetworkTostring.rkt")
(define (post socialnetwork)
  (lambda (date) (lambda (contenido . usuarios);(list socialnetwork date contenido usuarios)AGREGAR ELIMINA REPETIDOS
          (if (null? (car usuarios))
              (publicaEnUno socialnetwork (getUserOnlineSN socialnetwork) date contenido )
              (aplicaPublicaUser socialnetwork (getUserOnlineSN socialnetwork) (remove-duplicates (car usuarios)) date contenido)
              ))))

;(setListaPost sn post) se podría para hacerle un encrip al ultimo elemento de la listaPost (encriptaPost(traducePost (getUltimoElemento (getListaPost sn))))
;(setListaPost sn (encriptaPost(traducePost (getUltimoElemento (getListaPost sn)))))
;

(define(share socialnetwork)
  (lambda (date) (lambda (postID . usuarios)
                   (if (null? usuarios)
                       (aplicaSetUserPost socialnetwork (getUserOnlineSN socialnetwork)
                                   setListaPostUser (((postShare (getUserOnlineSN socialnetwork))date)
                                                     (getElemID (getListaPost socialnetwork) postID)))
                       (aplicaShareToAmigo socialnetwork (getUserOnlineSN socialnetwork)date usuarios postID)))))

(define(follow socialnetwork)
  (lambda (date)(lambda (user2)
    (if(and(socialnetwork? socialnetwork)
           (date? date)
           (registradoSN? socialnetwork user2))
            (if (not(sonAmigos? user2 (getAmigos(getUserOnlineSN socialnetwork))))
                (turnOff(aplicaSetUserPost socialnetwork (getUserOnlineSN socialnetwork)
                                   setListaAmigos (getTdaUser socialnetwork user2)))
                
                 (turnOff socialnetwork))
            #f) 
       )))

(define (register socialnetwork date username password)
  (if (and (socialnetwork? socialnetwork)(date? date) (string? username)(string? password))
      (if(registradoSN? socialnetwork username)
         (display "Este usuario se encuentra registrado. \n Ingrese de nuevo la funcion")
         (if(socialnetwork? (list (getNameSN socialnetwork)(getDateSN socialnetwork)
                                  (getFnEnc socialnetwork)(getFnDesc socialnetwork)
                                  (appendLista (registra socialnetwork date username password)(getListaUser socialnetwork))
                                  (getListaPost socialnetwork)))
           (list (getNameSN socialnetwork)(getDateSN socialnetwork)
                                  (getFnEnc socialnetwork)(getFnDesc socialnetwork)
                                  (appendLista (registra socialnetwork date username password)(getListaUser socialnetwork))
                                  (getListaPost socialnetwork))
            (display "bandera"))
         )
      (display "bandera2")))
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
   ;[(aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online")]
   ;[(display "hola")]
   ;[(operation (aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online") )]
   #|[(verificaLogin username password (getListaUser socialnetwork))
    (if (eqv? operation "socialnetwork->string")
        (operation (aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))
        (((operation (aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))date)param1 param2))]|#))))|#

(define(login socialnetwork username password operation)(lambda (date)(lambda (param1 . param2)
  (cond
   [(not(socialnetwork? socialnetwork)) (error "Ingrese un socialnetwork valido")]
   [(not(date? date)) (error "Ingrese una fecha valida")]
   [(not(string? username))(error "Ingrese username que sea string")]
   [(not(string? password)) (error "Ingrese password que sea string")]
   ;[(not(password? password username)) (error "Ingrese password distinto al username, de largo minimo 6 que contengas numero y letras")]
   [(not(>= (string-length username)4))(error "Ingrese un Username de minimo 4 caracteres")]
   ;[(contieneSpace (string->list username))(error "Ingrese un Username sin espacios")]
   ;[(contieneSpace (string->list password))(error "Ingrese un password sin espacios")]
   [(not(registradoSN? socialnetwork username))(error "El usuario no esta registrado")]
   ;[(operation(aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))]))))
   ;[(((operation(aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))date)user2)]))))
   [(eqv? operation follow)
    (((operation(aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))date)param1)]
   [(eqv? operation share)
    (((operation(aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))date)param1 (car param2))]
   [(eqv? operation post)(((operation(aplicaSetUserPost socialnetwork (getTdaUser socialnetwork username) setEstado "online"))date)param1 param2)]
   ))))
#|
(define (encryptFunction sn)
  (if(socialnetwork? sn)
     (encriptaLista )))|#
(define(socialnetwork->string sn)
  (if(socialnetwork? sn)
     (string-append "Bienvenido a " (getNameSN sn) "\n"
                    "Inicio:" "\n"
                    (list->string(getListaPost(desencriptaListaPostSN sn)))"\n"
                    "Perfil de Usuarios Registrados" "\n" "\n"
                    (traduceListaUser (getListaUser sn))
                    )
     #f))
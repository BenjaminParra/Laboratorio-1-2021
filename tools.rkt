#lang racket
(require "post.rkt")
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "Date.rkt")


(provide registra)
(provide postShare)
(provide aplicaShareToAmigo)


(provide verificaLogin)

(provide desencriptaListaPost)

(provide traducePost)
(provide encryptFunction)
(provide desencriptaLista)
(provide agregaElem)
(provide traduceListaPost)
(provide traduceListaUser)
(provide decryptFunction)
;(provide encriptaListaPostSN)
(provide remove-elem)
;sn user funcion variableaux

;##################### FUNCIONES PARA EL SHARE##############################
;Funcion que construye un TDA postShare que es una representacion de que el usuario compartió una publicacion en el perfil de un amigo
;DOM: tda user x tda fecha x tda post x tda amigo
;REC: En caso de que no se ingrese un tda amigo el recorrido será
;lista tda user x tda fecha x tda post x tda amigo, en caso contrario
;lista tda user x tda fecha x tda post
(define(postShare user)(lambda (date)
  (lambda (post . amigo)
    (if (null? amigo)
        (list user date post)
        (list user date post (car amigo))))))
;Funcion que se encarga de compartir desde la lista de publicaciones de la redsocial una publicacion especifica  en el muro de un usuario 
;DOM: tda sn x tda usuario x tda fecha x string x int
;REC: tda socialnetwork (con la publicacion hecha en el muro de la persona y en el inicio de la red social)
(define(shareToAmigo sn user date amigo ID)
  (if (and(socialnetwork? sn) (>= ID 1) (validaUsuario user) (string? amigo)(registradoSN? sn amigo))
      (aplicaSetUserPost (setListaPost sn (((postShare user)date)
                                                     (getElemID (getListaPost sn) ID)amigo))
                         (getTdaUser sn amigo) setListaPostUser (((postShare user)date)(getElemID (getListaPost sn) ID)amigo))
      #f))

;Funcion encarga de aplicar la funcion shateToAmigo a una lista de receptores
;DOM: tda sn x tda usuario x tda fecha x lista string x int
;REC: tda socialnetwork
(define (aplicaShareToAmigo sn user date listaReceptor ID)
   (if(null? listaReceptor)
     sn
     (if (and(socialnetwork? sn)(validaUsuario user)(date? date)(sonAmigos? (car listaReceptor) (getAmigos  user)))
         (aplicaShareToAmigo(shareToAmigo sn user date (car listaReceptor) ID)user date(cdr listaReceptor) ID)
         (aplicaShareToAmigo sn user date (cdr listaReceptor) ID))))


#|(define(existeEnLista user listaUser)
  (if (null? listaUser)
      #f
      (if (eqv? user (car listaUser))
          #t
          (existeEnLista user (cdr listaUser)))))|#

;#####################;##################### ;##################### ;##################### ;##################### ;#####################

;#####################;#####################;##################### FUNCIONES PARA EL REGISTER ;#####################;#####################
;Funcion que dado un nombre y contraseña se registra en un tda socialnetwork
;DOM: Tda socialnetwork x tda fecha x string x string
;REC: tda usuario
(define (registra sn date username password)
  (cond
   [(not(socialnetwork? sn)) (error "Ingrese un socialnetwork valido")]
   [(not(date? date)) (error "Ingrese una fecha valida")]
   [(not(string? username))(error "Ingrese username que sea string")]
   [(not(string? password)) (error "Ingrese password que sea string")]
   [(not(password? password username)) (error "Ingrese password distinto al username, de largo minimo 6 que contengas numero y letras")]
   [(not(>= (string-length username)4))(error "Ingrese un Username de minimo 4 caracteres")]
   [(contieneSpace (string->list username))(error "Ingrese un Username sin espacios")]
   [(contieneSpace (string->list password))(error "Ingrese un password sin espacios")]
   [(inicializaUser username password '()'() date "online")]))

;(char-numeric? #\0)


;descripción: Funcion verifica que una password sea de largo minimo 6, que no sea igual al nombre y que tengas al menos un numero
;dom: string x string
;rec: boolean
(define (password? password username)
  (if(string-ci=? password username)
     #f
     (if (and (>= (length (string->list password))6)(contieneNumero (string->list password))(contieneChar (string->list password)))
         #t
         #f)))

;descripción: Funcion que verifica que una lista de char contenga un caracter numero
;dom: lista char
;rec: boolean
(define(contieneNumero lista)
  (if (existeEnLista #t (map char-numeric? lista))
          #t
          #f)
  )
;descripción: Funcion que verifica que una lista de char contenga un caracter 
;dom: lista char
;rec: boolean
(define(contieneChar lista)
  (if (existeEnLista #t (map char-alphabetic? lista))
          #t
          #f)
  )
;descripción: Funcion que verifica que una lista de char contenga un caracter espacio 
;dom: lista char
;rec: boolean
(define(contieneSpace lista)
  (if (existeEnLista #\space lista)
      #t
      #f))
;#####################;#####################;#####################;#####################;#####################;#####################
;#####################;##################### FUNCIONES PARA EL LOGIN #####################;#####################;#####################
;Funcion que se valida el inicio de sesion
;DOM: string x string x lista tda user
;REC: boolen
(define(verificaLogin user pass listaUser)
  (if(null? listaUser)
     #f;(display "Usuario no encontrado")
     (if (eqv? user (car (getListaNames listaUser)))
         (if (eqv? pass (getPassword (car listaUser)))
             #t;(setEstado (car listaUser) "online")
             #f);(display "Clave incorrecta"))
     (verificaLogin user pass (cdr listaUser)))))
;#####################;#####################;#####################;#####################;#####################;#####################

;Funcion que da un string representativo del tda usuario
;DOM: tda user
;REC: string
(define(traduceUser user)
  (cond
    [(and (null? (getListaPostUser user))(null? (getAmigos user)))(string-append "El Usuario " (getUser user)" Su fecha de registro es:" (traduceDate (getDate user)) "\n" " no tiene amigos y no ha realizado alguna publicacion")]
    [(and (null? (getListaPostUser user))(not(null? (getAmigos user))))(string-append "El Usuario " (getUser user)
                                            " sus amigos son "
                    (traduceAmigos (getListaNames(getAmigos user))) ", su perfil no contiene ninguna publicacion. ""El Usuario se encuentra "
                                                      (getEstado user)"\n")]
    [(and (not(null? (getListaPostUser user)))(null? (getAmigos user)))(string-append "El Usuario " (getUser user)" Su fecha de registro es:" (traduceDate (getDate user)) "\n"
                                            " no tiene amigos registrados, su perfil contiene:" "\n" "\n" (traduceListaPost (getListaPostUser user))"El Usuario se encuentra "
                                                      (getEstado user)"\n")]
    
    [(string-append "El usuario " (getUser user)" Su fecha de registro es:" (traduceDate (getDate user)) "\n" "Sus amigos son "
                    (traduceAmigos (getListaNames(getAmigos user)))"\n"   "Su perfil contiene: " "\n" "\n"(traduceListaPost (getListaPostUser user)) "El Usuario se encuentra "
                                                      (getEstado user)"." "\n" )]))
;Funcion que aplica la funcion traduceUser a uns lista de tda User
;DOM: lista Tda user
;REC: string
(define(traduceListaUser listauser)
  (if (null? listauser)
      ""
      (string-append(string-append (traduceUser (car listauser))"\n")(traduceListaUser (cdr listauser)))))
;Funcion que da un string representativo de los amigos del tda usuario
;DOM: lista string
;REC: string
(define(traduceAmigos amigosUser)
  (if (null? amigosUser)
      ""
      (if (= 1 (length amigosUser))
          (string-append(car amigosUser) ".")
          (string-append (string-append(car amigosUser) ",")(traduceAmigos (cdr amigosUser))))))
;;Funcion que da un string representativo de un tda publicacion
;DOM: tda publicacion
;REC: string
(define(traducePost post);post trae tda user,
(if (not(string? (caddr post)))
    (if(= 3 (length post))
       (string-append "El usuario " (getUser(car post)) " ha compartido el post " "\n"
                                                 "<<" (list->string(desencriptaLista(caddr post)))">>" "en su perfil."
                                                  "\n" "Fecha:" (traduceDate (cadr post))"\n" )
       (cond
    [(null? (cadddr post))(string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                  "." "\n" "Fecha:" (traduceDate (cadr post))"\n")]
    [(not(null? (cadddr post)))(if(string?(caddr post));si hay receptor
                                  (string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                 " en el perfil de " (getUser(car(cadddr post))) "." "\n" "Fecha:" (traduceDate (cadr post))"\n"#|(listToString(cadr post)"")|#)
                                  (if(= 4 (length post))
                                     (string-append "El usuario " (getUser(car post)) " ha compartido el post " "\n"
                                                 "<<" (list->string(desencriptaLista(caddr post)))">>" "en el perfil de "
                                                 (cadddr post)"." "\n" "Fecha:" (traduceDate (cadr post))"\n" )
                                     (string-append "El usuario " (getUser(car post)) " ha compartido el post " "\n"
                                                 "<<" (list->string(desencriptaLista(caddr post)))">>" "en su perfil." "\n" "Fecha:" (traduceDate (cadr post))"\n" ))
                                  )]))
    
    
      (cond
    [(null? (cadddr post))(string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                  "." "\n" "Fecha:" (traduceDate (cadr post))"\n")]
    [(not(null? (cadddr post)))(if(string?(caddr post))
                                  (string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                 " en el perfil de " (getUser(car(cadddr post))) "." "\n" "Fecha:" (traduceDate (cadr post))"\n"#|(listToString(cadr post)"")|#)
                                  
                                  (string-append "El usuario " (getUser(car post)) " ha compartido el post " "\n"
                                                 "<<"(list->string(traducePost (caddr post)))">>" "en el perfil de "
                                                 (cadddr post)"." "\n" "Fecha:" (traduceDate (cadr post))"\n" ))])
      )
  
      )
  
  
  
;Funcion que da un string representativo del tda fecha
;dom: tda fecha
;rec: string
(define (traduceDate date)
  (if (date? date)
      (if (null? date)
          #f
          (string-append (number->string (getDD date))"/"(number->string (getMM date))"/"(number->string (getYY date))"."))
      #f))
;Funcion que aplica el traducePost sobre una lista de tda publicaciones
;dom: lista tda publicacion
;rec: string
(define(traduceListaPost listaPost)
  (if (null? listaPost)
      ""
      (string-append (string-append(traducePost (car listaPost))"\n")(traduceListaPost (cdr listaPost)))))

#|
(define(encriptaListaPost listaPost)
  (if (null? listaPost)
      listaPost
      (append (encriptaLista (string->list (traducePost (car listaPost))))(encriptaListaPost (cdr listaPost)))))|#

(define(desencriptaListaPost listaPost)
  (if (null? listaPost)
      listaPost
      (append (desencriptaLista (string->list (traducePost (car listaPost))))(desencriptaListaPost (cdr listaPost)))))
#|(define(encriptaListaPostSN sn)
  (if (socialnetwork? sn)
      (setListaPostSN sn (encriptaListaPost (getListaPost sn)))
      #f))|#
;Funcion que desencripta las publicaciones de la red social
;dom: tda red social
;rec: tda red social
(define(decryptFunction sn);desencripfuction
  (if (socialnetwork? sn)
      (setListaPostSN sn (desencriptaLista (getListaPost sn)))
      #f))


;Funcion que desencripta una lista de caracteres (string encriptado)
;dom: lista de caracteres
;rec: lista caracteres
(define (desencriptaLista listaWord)
  (if (null? listaWord)
      ;(agregaElem "&" listaWord)
      listaWord
      (cond
      [(char-ci=? (car listaWord) #\z)(cons #\a(desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\y)(cons #\b (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\x)(cons #\c(desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\w)(cons #\d(desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\v)(cons #\e (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\u)(cons #\f (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\t)(cons #\g (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\s)(cons #\h (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\r)(cons #\i (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\q)(cons #\j (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\p)(cons #\k (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\o)(cons #\l (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\n)(cons #\m (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\m)(cons #\n (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\%)(cons #\ñ (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\l)(cons #\o (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\k)(cons #\p (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\j)(cons #\q (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\i)(cons #\r (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\h)(cons #\s (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\g)(cons #\t (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\f)(cons #\u (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\e)(cons #\v (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\d)(cons #\w (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\c)(cons #\x (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\b)(cons #\y (desencriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\a)(cons #\z (desencriptaLista (cdr listaWord)))]
      [(cons (car listaWord)(desencriptaLista (cdr listaWord)))])))

;(setListaPost sn (encriptaLista(string->list(traducePost (getUltimoElemento (getListaPost sn))))))
;Funcion que se encarga de encriptar una lista de caracteres (string transformado a una lista de caracteres)
;dom: lista de caracteres
;rec: lista de caracteres 
(define (encryptFunction listaWord);encrip
  (if (null? listaWord)
      ;(agregaElem "&" listaWord)
      listaWord
      (cond
      [(char-ci=? (car listaWord) #\a)(cons #\z(encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\b)(cons #\y (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\c)(cons #\x (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\d)(cons #\w (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\e)(cons #\v (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\f)(cons #\u (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\g)(cons #\t (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\h)(cons #\s (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\i)(cons #\r (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\j)(cons #\q (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\k)(cons #\p (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\l)(cons #\o (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\m)(cons #\n (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\n)(cons #\m (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\ñ)(cons #\% (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\o)(cons #\l (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\p)(cons #\k (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\q)(cons #\j (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\r)(cons #\i (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\s)(cons #\h (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\t)(cons #\g (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\u)(cons #\f (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\v)(cons #\e (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\w)(cons #\d (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\x)(cons #\c (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\y)(cons #\b (encryptFunction (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\z)(cons #\a (encryptFunction (cdr listaWord)))]
      [(cons (car listaWord)(encryptFunction (cdr listaWord)))])))




#|(encriptaLista (string->list (traducePost(car (getListaPostUser (car '(("benja"
   "123"
   (("chilo" "123" () () (1 2 2021) "offline") ("juli" "123" () () (1 2 2021) "offline"))
   ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ())
    (("benja"
      "123"
      (("chilo" "123" () () (1 2 2021) "offline") ("juli" "123" () () (1 2 2021) "offline"))
      ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ()))
      (1 2 2021)
      "online")
     (1 2 2021)
     "prueba.audio"
     ()))
   (1 2 2021)
   "online")
  ("chilo" "123" () () (1 2 2021) "offline")
  ("juli" "123" () () (1 2 2021) "offline"))))))))|#

;Funcion que agrega un elemento para encriptar una lista
(define (agregaElem elem lista)
  (if(null? lista)
     lista
     (append (list (car lista) elem)(agregaElem elem (cdr lista)))))
;Funcion que elimina un elemento para desencriptar una lista
(define (remove-elem elem lista)
  (if (null? lista)
      lista
      (if (eqv? elem (car lista))
          (remove-elem elem (cdr lista))
          (cons (car lista)(remove-elem elem (cdr lista))))))

#|(string->list (getContenidoPost (car(getListaPostUser (car '(("benja"
   "123"
   (("chilo" "123" () () (1 2 2021) "offline") ("juli" "123" () () (1 2 2021) "offline"))
   ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ())
    (("benja"
      "123"
      (("chilo" "123" () () (1 2 2021) "offline") ("juli" "123" () () (1 2 2021) "offline"))
      ((("benja" "123" () () (1 2 2021) "online") (1 2 2021) "hola123.text" ()))
      (1 2 2021)
      "online")
     (1 2 2021)
     "prueba.audio"
     ()))
   (1 2 2021)
   "offline")
  ("chilo" "123" () () (1 2 2021) "offline")
  ("juli" "123" () () (1 2 2021) "offline")))))))|#

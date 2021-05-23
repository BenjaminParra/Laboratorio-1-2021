#lang racket
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "login.rkt")
(require "Date.rkt")
(require "post.rkt")

(provide encriptaLista)
(provide agregaElem)
(provide traduceListaPost)
(provide traduceListaUser)
(provide desencriptaListaPostSN)

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
                                            " no tiene amigos registrados, su perfil contiene:" "\n" "\n" (traduceListaPost (getListaPostUser user)))]
    
    [(string-append "El usuario " (getUser user) " sus amigos son "
                    (traduceAmigos (getListaNames(getAmigos user)))"\n"   "Su perfil contiene: " "\n" "\n"(traduceListaPost (getListaPostUser user)) "El Usuario se encuentra "
                                                      (getEstado user)"\n")]))
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
#|
(define(traduceAmigos amigosUser)
 (if(null? amigosUser)
    "."
    (string-append (string-append(car amigosUser) "\n")(traduceAmigos (cdr amigosUser)))))
|#

(define(traduceAmigos amigosUser)
  (if (null? amigosUser)
      ""
      (if (= 1 (length amigosUser))
          (string-append(car amigosUser) ".")
          (string-append (string-append(car amigosUser) ",")(traduceAmigos (cdr amigosUser))))))
(define(traducePost post);post trae tda user, 
  
  (if (null? (cadddr post))
      (string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                  "." "\n" "Fecha:" (traduceDate (cadr post))"\n")
      (string-append "El usuario " (getUser(car post)) " ha publicado " (caddr post)
                 " en el perfil de " (getUser(car(cadddr post))) "." "\n" "Fecha:" (traduceDate (cadr post))"\n"#|(listToString(cadr post)"")|#))
  )

(define (traduceDate date)
  (if (date? date)
      (if (null? date)
          #f
          (string-append (number->string (getDD date))"/"(number->string (getMM date))"/"(number->string (getYY date))"."))
      #f))
(define(traduceListaPost listaPost)
  (if (null? listaPost)
      ""
      (string-append (string-append(traducePost (car listaPost))"\n")(traduceListaPost (cdr listaPost)))))

(define (encriptaPost post)
  (post? post)
  (list (getUserPost post) (getDatePost post) (getContenidoPost post)(getListaUserPost post)))


(define(encriptaListaPost listaPost)
  (if (null? listaPost)
      listaPost
      (append (encriptaLista (string->list (traducePost (car listaPost))))(encriptaListaPost (cdr listaPost)))))

(define(desencriptaListaPost listaPost)
  (if (null? listaPost)
      listaPost
      (append (desencriptaLista (string->list (traducePost (car listaPost))))(desencriptaListaPost (cdr listaPost)))))
(define(encriptaListaPostSN sn)
  (if (socialnetwork? sn)
      (setListaPostSN sn (encriptaListaPost (getListaPost sn)))
      #f))

(define(desencriptaListaPostSN sn)
  (if (socialnetwork? sn)
      (setListaPostSN sn (desencriptaLista (getListaPost sn)))
      #f))


;desencriptaLista

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



(define (encriptaLista listaWord)
  (if (null? listaWord)
      ;(agregaElem "&" listaWord)
      listaWord
      (cond
      [(char-ci=? (car listaWord) #\a)(cons #\z(encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\b)(cons #\y (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\c)(cons #\x (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\d)(cons #\w (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\e)(cons #\v (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\f)(cons #\u (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\g)(cons #\t (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\h)(cons #\s (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\i)(cons #\r (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\j)(cons #\q (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\k)(cons #\p (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\l)(cons #\o (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\m)(cons #\n (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\n)(cons #\m (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\ñ)(cons #\% (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\o)(cons #\l (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\p)(cons #\k (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\q)(cons #\j (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\r)(cons #\i (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\s)(cons #\h (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\t)(cons #\g (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\u)(cons #\f (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\v)(cons #\e (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\w)(cons #\d (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\x)(cons #\c (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\y)(cons #\b (encriptaLista (cdr listaWord)))]
      [(char-ci=? (car listaWord) #\z)(cons #\a (encriptaLista (cdr listaWord)))]
      [(cons (car listaWord)(encriptaLista (cdr listaWord)))])))




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
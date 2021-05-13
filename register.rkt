#lang racket
(require "socialNetwork.rkt")
(require "tdaUsuario.rkt")
(require "Date.rkt")
;(require "post.rkt")

(provide (all-defined-out))

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
            (display "caca"))
         )
      (display "aca")))

#|(setUser (register (register (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn")
 (date 1 3 21) "benja" "benja123")(date 1 3 21) "chilo" "benja123")(user "benja" "benja123" '() '() '(1 3 21) "online")setPassword "comelo123")|#
#|(setUser (register (register (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn")
 (date 1 3 21) "benja" "benja123")(date 1 3 21) "chilo" "benja123")(user "benja" "benja123" '() '() '(1 3 21) "online")setListaPostUser'("este es un post de pana"))|#
  
;(user "Benja" "123" '() '() (date 01 02 2021) "online")
;(registra (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn") (date 1 3 21) "benja" "benja123")
#|
(define (registra sn date username password)
  (if(and (socialnetwork? sn)(date? date) (string? username)(string? password)(password? password username))
     (user username password '()'() date)
     #f))|#
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

;Password debe tener numeros y letras, largo minimo 6

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

(define(contieneSpace lista)
  (if (existeEnLista #\space lista)
      #t
      #f))

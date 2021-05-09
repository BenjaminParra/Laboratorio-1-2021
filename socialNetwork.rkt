#lang racket
;TDA SOCIAL
(require "tdaUsuario.rkt")
(require "Date.rkt")
;(require "register.rkt")
(provide (all-defined-out))
#|REPRESENTACION
string X TDA date X funcionEncriptadora X funcionDesincrptadora
(list nombreRedSocial Fecha FuncEncriptadora FuncDesincriptadora|#

 ;(socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn")
#|
(define (redSocial name date usuarios publicaciones)
  (if (and(or (equal? "facebook" name)(equal? "instagram" name)(equal? "twitter" name))
      (map validaUsuario usuarios)(date? date));crear tda publicacion para verificar si son del tipo y asi poder hacer un list user publicaciones
             #t
             #f)
         )|#
(define (args . list)list)
;CONSTRUCTOR
;descripción: Permite crear una redSocial
;dom: string X tdaFecha X funcionEncriptadora X funcionDesincrptadora
;rec: lista (notese que hay 2 lista vacias la primera corresponde a lista de Usuarios y Lista De publicaciones
(define (socialnetwork name date encryptFunction decryptFunction)
  (if (and (string? name)(date? date))
      (args name date encryptFunction decryptFunction '() '())
      null))


(define (socialnetwork? socialnetwork)
  (and (or (string-ci=? "facebook" (car socialnetwork))
           (string-ci=? "fb" (car socialnetwork))
           (string-ci=? "instagram" (car socialnetwork))
           (string-ci=? "ig" (car socialnetwork))
           (string-ci=? "twitter" (car socialnetwork))
           (string-ci=? "tw" (car socialnetwork)))
       (= 6 (length socialnetwork))
       (list? (car(cdr(cdddr socialnetwork))))
       (list? (car(cddr(cdddr socialnetwork))))))


;SELECTORES
;descripción: Función que retorna el nombre de la socialNetwork
;dom: socialnetwork
;rec: string
(define (getNameSN socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car socialnetwork)
      0
      )
  )
;descripción: Función que retorna el tda fecha de la socialNetwork
;dom: socialnetwork
;rec: date
(define (getDateSN socialnetwork)
  (if (socialnetwork? socialnetwork)
      (cadr socialnetwork)
      0
      )
  )

(define (getFnEnc socialnetwork)
  (if (socialnetwork? socialnetwork)
      (caddr socialnetwork)
      0
      )
  )
(define (getFnDesc socialnetwork)
  (if (socialnetwork? socialnetwork)
      (cadddr socialnetwork)
      0
      )
  )
;descripción: Función que retorna la lista Tda usuarios de la socialNetwork
;dom: socialnetwork
;rec: lista tda usuario
(define (getListaUser socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car(cdr(cdddr socialnetwork)))
      0
      )
  )

;(car(cddr(cdddr socialnetwork)))))

;descripción: Función que retorna la lista tda post de la socialNetwork
;dom: socialnetwork
;rec: lista tda post
(define (getListaPost socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car(cddr(cdddr socialnetwork)))
      0
      )
  )
#|(list (getNameSN (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn"))(getDateSN (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn"))
                                  (getFnEnc (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn"))(getFnDesc (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn"))
                                  (appendLista (registra (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn") (date 25 10 2021) "benja"  "username123")(getListaPost (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn"))))|#
;Modificadores
;descripción: Función cambia el nombre de la redSocial
;dom: socialnetwork x string
;rec: socialnetwork
(define (setName sn newname)
  (if (socialnetwork? sn)
      (if (socialnetwork?(list newname (getDateSN sn)(getFnEnc sn)(getFnDesc sn)(getListaUser sn)(getListaPost sn)))
          (list newname (getDateSN sn)(getFnEnc sn)(getFnDesc sn)(getListaUser sn)(getListaPost sn))
          '()
          )
      '()
      )
  )
;descripción: Función cambia la fecha de la redSocial
;dom: socialnetwork x int x int x int
;rec: socialnetwork
(define (setDate sn dd mm yyyy)
  (if (socialnetwork? sn)
      (if (socialnetwork? (list (getNameSN sn) (date dd mm yyyy) (getFnEnc sn) (getFnDesc sn)(getListaUser sn)(getListaPost sn)))
          (list (getNameSN sn) (date dd mm yyyy) (getFnEnc sn) (getFnDesc sn)(getListaUser sn)(getListaPost sn))
          '())
      '()
      )
  )

(define (setListaPost sn post)
  (if (socialnetwork? sn)
      (if (socialnetwork? (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(getListaUser sn)
                                (appendLista post (getListaPost sn))))
          (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(getListaUser sn)
                                (appendLista post (getListaPost sn)))
          '()
          )
      '()
      )
  )

;descripción: Función que verifica si el nombre del user está registrado en el socialnetwork
;dom: socialnetwork x string
;rec: boolean
(define (registradoSN? sn user)
  (if (and(socialnetwork? sn)(string? user))
      (if(registradoLista? (getListaUser sn)user)
         #t
         #f)
      null)
  )

;descripción: Función auxiliar de registradoSN? que verifica si el un string se repite en una lista de usuarios en el socialnetwork
;dom: socialnetwork x string
;rec: boolean
(define (registradoLista? listaUser user)
  (if (and (string? user)(list? listaUser))
      (existeEnLista user (getListaNames listaUser))
      null))

;descripción: Funcion que en base a una lista de TDA user obtiene una lista de los nombres los usuarios
;dom: socialnetwork
;rec: lista strings
(define(getListaNames listaUser)
  (if (null? listaUser)
      listaUser
      (cons(getUser (car listaUser))(getListaNames (cdr listaUser)))))

;descripción: Funcion encapsuladora de fnAuxExiste 
;dom: elemento x lista
;rec: boolean
(define(existeEnLista elem lista)
  (if (null? lista)
      #f
      (fnAuxExiste elem lista)))
;descripción: Funcion que verifica si un string o un entero pertenece a una lista
;dom: string o entero X lista
;rec: boolean
(define(fnAuxExiste elem lista)
  (if (null? lista)
      #f
      (if (eqv? elem (car lista))
          #t
          (fnAuxExiste elem (cdr lista)))))
;descripción: Funcion añade una lista a una matriz (entiendase matriz como una lista de lista)
;dom: lista X matriz
;rec: matriz
(define (appendLista lista matriz)
  (if (and (list? lista)(null? matriz))
      (list  lista) 
      (cons (car matriz)(appendLista lista (cdr matriz)))))







     
;(appendLista(user "Benja" "123" '() '())(list(user "Benja" "123" '() '())(user "chilo" "123" '() '())(user "juli" "123" '() '())))
;compara string (string-ci=? "Que" "qUe")

  
;LISTA USER = (list(user "Benja" "123" '() '())(user "chilo" "123" '() '())(user "juli" "123" '() '()))
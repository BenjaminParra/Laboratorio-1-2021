#lang racket
(provide (all-defined-out))

(require "Date.rkt")
;TDA USUARIO
;user string pass string amigos lista de Usuarios, perfil lista de publicaciones
;cuando se registre un usuario se verificara que solo sean letras y numeros


;CONSTRUCTOR
;descripción: Permite crear un usuario
;dom: string X string X lista X lista x tda date x string
;rec: lista
(define (user user pass amigos perfil date estado)
  (if (and (string? user)(string? pass)
           (list? amigos);(empty? amigos)
           (list? perfil);(empty? perfil)
           (date? date)(string? estado))
      (list user pass amigos perfil date estado)
      null
      )
  )
;(list(user "Benja" "123" '() '()(date 01 02 2021)"off")(user "chilo" "123" '() '()(date 01 02 2021)"off")(user "juli" "123" '() '()(date 01 02 2021)"off"))
(define (inicializaUser user pass amigos perfil date estado)
  (if (and (string? user)(string? pass)
           (list? amigos)(empty? amigos)
           (list? perfil)(empty? perfil)
           (date? date)(string? estado))
      (list user pass amigos perfil date estado)
      null
      )
  )


;PERTENENCIA
;descripción: Función que permite determinar si un elemento cualquiera es del tipo usuario
;             se implementa a partir del constructor
;             evaluando el retorno
;dom: elemento de cualquier tipo
;rec: boolean
(define (validaUsuario usuario)
  ;(if (boolean? usuario)¬u¬ 
      ;#f
      (if (and (= 6 (length usuario))(not(= 0 (string-length (car usuario))))(not(= 0(string-length(cadr usuario))))(date? (car(cdr(cdddr usuario)))))
          #t
          #f
          )
      ;)
  )
;(user "Benja" "123" '() '())

;SELECTORES
;descripción: Función que retorna el nombre del usuario
;dom: usuario
;rec: string
(define (getUser usuario)
  (if (validaUsuario usuario)
      (car usuario)
      ""
      )
  )
;descripción: Función que retorna la password del usuario
;dom: usuario
;rec: string
(define (getPassword usuario)
  (if (validaUsuario usuario)
      (cadr usuario)
      ""
      )
  )

;descripción: Función que retorna la lista de amigos del usuario
;dom: usuario
;rec: lista
(define (getAmigos usuario)
  (if (validaUsuario usuario)
      (caddr usuario)
      0
      )
  )

;descripción: Función que retorna el perfil del usuario
;dom: usuario
;rec: lista
(define (getListaPostUser usuario)
  (if (validaUsuario usuario)
      (cadddr usuario)
      0
      )
  )
;(car(cdr(cdddr(user "Benja" "123" '() '() (date 01 02 2021)))))
;descripción: Función que retorna el date del usuario
;dom: usuario
;rec: date
(define (getDate usuario)
    (if (validaUsuario usuario)
      (car(cdr(cdddr usuario)))
      0
      )
  )
;descripción: Función que retorna el estado del usuario ya sea online u offline 
;dom: usuario
;rec: lista
(define (getEstado usuario)
    (if (validaUsuario usuario)
      (car(cddr(cdddr usuario)))
      0
      )
  )


;Modificadores
;descripción: Función que crea un nuevo usuario a partir de un usuario de entrada reemplazando el valor correspondiente al nombre del usuario
;dom: usuario x string
;rec: usuario
(define (setNombre usuario nombre)
  (if (validaUsuario usuario)
      (user nombre (getPassword usuario) (getAmigos usuario)(getListaPostUser usuario)(getDate usuario)(getEstado usuario))
      null
      )
  )
;(user "Benja" "123" '() '())
;descripción: Función que crea un nuevo usuario a partir de un usuario de entrada reemplazando el valor correspondiente a lacontraseña del usuario
;dom: usuario x string
;rec: usuario
(define (setPassword usuario password)
  (if (validaUsuario usuario)
      (user (getUser usuario) password (getAmigos usuario)(getListaPostUser usuario)(getDate usuario)(getEstado usuario))
      null
      )
  )

(define (setListaPostUser usuario post)
  (if (validaUsuario usuario)
      (user (getUser usuario) (getPassword usuario) (getAmigos usuario)(appendListaUser post (getListaPostUser usuario))
            (getDate usuario)(getEstado usuario))
      null
          )
      
      )

(define (setListaAmigos usuario amigo)
  (if (validaUsuario usuario)
      (user (getUser usuario) (getPassword usuario) (appendListaUser amigo(getAmigos usuario))(getListaPostUser usuario)
            (getDate usuario)(getEstado usuario))
      null
          )
      
      )
  


;descripción: Función que cambia el estado del usuario
;dom: usuario x string
;rec: usuario
(define (setEstado usuario estado)
  (if (validaUsuario usuario)
      (user (getUser usuario) (getPassword usuario) (getAmigos usuario)(getListaPostUser usuario)(getDate usuario) estado)
      null
      )
  )

#| Funcion que se encarga de tranforma una fila a string
 Entrada: fila x string
 Salida: String|#
(define(listToString lista str)
  (if (null? lista)
      str
      (if(string? (car lista))
         (listToString (cdr lista)(string-append str " " (car lista)))
         (listToString (cdr lista)(string-append str " "(number->string(car lista)))))
      )
  )

;descripción: Funcion añade una lista a una matriz (entiendase matriz como una lista de lista)
;dom: lista X matriz
;rec: matriz
(define (appendListaUser lista matriz)
  (if (or (list? lista)(null? matriz))
      (list  lista) 
      (cons (car matriz)(appendListaUser lista (cdr matriz)))))


(define (post . args) args)


      
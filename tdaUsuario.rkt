#lang racket
(provide (all-defined-out))
;TDA USUARIO
;user string pass string amigos lista de Usuarios, perfil lista de publicaciones
;cuando se registre un usuario se verificara que solo sean letras y numeros


;CONSTRUCTOR
;descripción: Permite crear un usuario
;dom: string X string X lista X lista
;rec: lista
(define (creaUsuario user pass amigos perfil)
  (if (and (string? user)(string? pass)
           (list? amigos)(empty? amigos)
           (list? perfil)(empty? perfil))
      (list user pass amigos perfil)
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
  ;(if (boolean? usuario)
      ;#f
      (if (and (= 4 (length usuario))(not(= 0 (string-length (car usuario))))(not(= 0(string-length(cadr usuario)))))
          #t
          #f
          )
      ;)
  )
;(creaUsuario "Benja" "123" '() '())

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
(define (getPerfil usuario)
  (if (validaUsuario usuario)
      (cadddr usuario)
      0
      )
  )


;Modificadores
;descripción: Función que crea un nuevo usuario a partir de un usuario de entrada reemplazando el valor correspondiente al nombre del usuario
;dom: usuario x string
;rec: usuario
(define (setNombre usuario nombre)
  (if (validaUsuario usuario)
      (creaUsuario nombre (getPassword usuario) (getAmigos usuario)(getPerfil usuario))
      null
      )
  )
;(creaUsuario "Benja" "123" '() '())
;descripción: Función que crea un nuevo usuario a partir de un usuario de entrada reemplazando el valor correspondiente a lacontraseña del usuario
;dom: usuario x string
;rec: usuario
(define (setPassword usuario password)
  (if (validaUsuario usuario)
      (creaUsuario (getUser usuario) password (getAmigos usuario)(getPerfil usuario))
      null
      )
  )

    
      
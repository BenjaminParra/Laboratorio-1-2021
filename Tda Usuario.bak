#lang racket
;TDA USUARIO
;user string pass string amigos lista de Usuarios, perfil lista de publicaciones
;cuando se registre un usuario se verificara que solo sean letras y numeros
(define (creaUsuario user pass amigos perfil)
  (if (and (string? user)(string? pass)
           (list? amigos)(empty? amigos)
           (list? perfil)(empty? perfil))
      (list user pass amigos perfil)
      #f
      )
  )

(define (validaUsuario usuario)
  (if (boolean? usuario)
      #f
      (if (and (= 4 (length usuario))(not(= 0 (string-length (car usuario))))(not(= 0(string-length(cadr usuario)))))
          #t
          #f
          )
      )
  )
      
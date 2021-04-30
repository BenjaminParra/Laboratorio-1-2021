#lang racket
;TDA SOCIAL
(require "tdaUsuario.rkt")
(provide (all-defined-out))
#||#
 ;(socialnetwork “fb” (date 25 10 2021) encryptFn encryptFn)

#|(define (socialnetwork name date encFun decFun)
         (lambda (redSocial name usuarios publicaciones)
           (display "fuck")))|#
(define (redSocial name usuarios publicaciones)
  (if (or (equal? "facebook" name)(equal? "instagram" name)(equal? "twitter" name))
      (if (map validaUsuario usuarios);crear tda publicacion para verificar si son del tipo y asi poder hacer un list user publicaciones
          #t
          #f)
      #f)
  )


  



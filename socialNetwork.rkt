#lang racket
;TDA SOCIAL
(require "tdaUsuario.rkt")
(require "Date.rkt")
(provide (all-defined-out))
#|REPRESENTACION
string X TDA date X funcionEncriptadora X funcionDesincrptadora
(list nombreRedSocial Fecha FuncEncriptadora FuncDesincriptadora|#

 ;(socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn")

(define (redSocial name date usuarios publicaciones)
  (if (and(or (equal? "facebook" name)(equal? "instagram" name)(equal? "twitter" name))
      (map validaUsuario usuarios)(date? date));crear tda publicacion para verificar si son del tipo y asi poder hacer un list user publicaciones
             #t
             #f)
         )
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
  (and (or (equal? "facebook" (car socialnetwork))
           (equal? "instagram" (car socialnetwork))
           (equal? "twitter" (car socialnetwork)))
       (= 6 (length socialnetwork))
       (list? (car(cdr(cdddr socialnetwork))))
       (list? (car(cddr(cdddr socialnetwork))))))

(define (getNameSN socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car socialnetwork)
      0
      )
  )

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

(define (getListaUser socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car(cdr(cdddr socialnetwork)))
      0
      )
  )

;(car(cddr(cdddr socialnetwork)))))
(define (getListaPost socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car(cddr(cdddr socialnetwork)))
      0
      )
  )

(define (setName sn newname)
  (if (socialnetwork? sn)
      (socialnetwork newname (getDateSN sn)(getFnEnc sn)(getFnDesc sn)(getListaUser sn)(getListaPost sn))
      '()))

(define (setDate sn dd mm yyyy)
  (if (socialnetwork? sn)
      (socialnetwork (getNameSN sn) (date dd mm yyyy) (getFnDesc sn)(getListaUser sn)(getListaPost sn))
      '()))




(define (suma x y)(lambda(a)(+ x y a)))

  

#lang racket
(provide (all-defined-out))
;(require "post.rkt")
(require "tdaUsuario.rkt")
(require "socialNetwork.rkt")
(require "Date.rkt")
;sn user funcion variableaux
(define(share socialnetwork)
  (lambda (date) (lambda (postID . usuarios)
                   (if (null? usuarios)
                       (aplicaSetUserPost socialnetwork (getUserOnlineSN socialnetwork)
                                   setListaPostUser (((postShare (getUserOnlineSN socialnetwork))date)
                                                     (getElemID (getListaPost socialnetwork) postID)))
                       (aplicaShareToAmigo socialnetwork (getUserOnlineSN socialnetwork)date usuarios postID)))))
                   

(define(postShare user)(lambda (date)
  (lambda (post . amigo)
    (if (null? amigo)
        (list user date post)
        (list user date post (car amigo))))))
;aplicaPublicaEnDiversos y estamos
#|Si son amigos puedo compartir su muro |#
(define(shareToAmigo sn user date amigo ID)
  (if (and(socialnetwork? sn) (>= ID 1) (validaUsuario user) (string? amigo)(registradoSN? sn amigo))
      (aplicaSetUserPost (setListaPost sn (((postShare user)date)
                                                     (getElemID (getListaPost sn) ID)amigo))
                         (getTdaUser sn amigo) setListaPostUser (((postShare user)date)(getElemID (getListaPost sn) ID)amigo))
      #f))

(define (aplicaShareToAmigo sn user date listaReceptor ID)
   (if(null? listaReceptor)
     sn
     (if (and(socialnetwork? sn)(validaUsuario user)(date? date)(sonAmigos? (car listaReceptor) (getAmigos  user)))
         (aplicaShareToAmigo(shareToAmigo sn user date (car listaReceptor) ID)user date(cdr listaReceptor) ID)
         (aplicaShareToAmigo sn user date (cdr listaReceptor) ID))))

;(appendListaUser (getListaPostUSer (getUserOnlineSN socialnetwork)) (getElemID (getListaPost socialnetwork) postID))

;(aplicaSetUserPost socialNetwork (getUserOnlineSN socialnetwork) funcion variableaux)

(define(existeEnLista user listaUser)
  (if (null? listaUser)
      #f
      (if (eqv? user (car listaUser))
          #t
          (existeEnLista user (cdr listaUser)))))


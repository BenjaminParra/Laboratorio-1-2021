#lang racket
;TDA SOCIAL
(require "tdaUsuario_19741126_ParraLuman.rkt")
(require "Date_19741126_ParraLuman.rkt")
;(requiere "tools.")
(provide (all-defined-out))

#|REPRESENTACION
string X TDA date X funcionEncriptadora X funcionDesincrptadora
(list nombreRedSocial Fecha FuncEncriptadora FuncDesincriptadora|#

;(socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn" listaUser listaPost)

(define (args . list)list)
;CONSTRUCTOR
;descripción: Permite crear una redSocial
;dom: string X tdaFecha X funcionEncriptadora X funcionDesincrptadora
;rec: lista (notese que hay 2 lista vacias la primera corresponde a lista de Usuarios y Lista De publicaciones
(define (socialnetwork name date encryptFunction decryptFunction)
  (if (and (string? name)(date? date))
      (args name date encryptFunction decryptFunction '() '())
      null))

;Funcion de pertenencia que valida el nombre del socialnetwork
;dom: tda socialnetwork
;rec: boolean
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
;descripción: Función que retorna la funcion encriptadora de la socialNetwork
;dom: socialnetwork
;rec: funcion encriptadora
(define (getFnEnc socialnetwork)
  (if (socialnetwork? socialnetwork)
      (caddr socialnetwork)
      0
      )
  )
;descripción: Función que retorna la funcion desencriptadora de la socialNetwork
;dom: socialnetwork
;rec: funcion desencriptadora
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

;descripción: Función que retorna la lista tda post de la socialNetwork
;dom: socialnetwork
;rec: lista tda post
(define (getListaPost socialnetwork)
  (if (socialnetwork? socialnetwork)
      (car(cddr(cdddr socialnetwork)))
      0
      )
  )

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

;Funcion que obtiene el ultimo elemento de una lista
;dom: lista
;rec: elemento lista
(define(getUltimoElemento lista)
  (if (= 1 (length lista))
      (car lista)
      (getUltimoElemento (cdr lista))))

;Funcion que agrega un tda post a la lista de publicaciones del tda sn
;dom: tda sn x tda publicacion
;rec: tda sn
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

;Funcion que remplaza el ultimo elemento de un sn con otro tda post
;dom: tda sn x tda post
;rec: tda sn
(define(replacePost sn post)
  (if (socialnetwork? sn)
      (if (socialnetwork? (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(getListaUser sn)
                                (appendLista post (getListaPost sn))))
          (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(getListaUser sn)
                                (appendLista post (remove-elemSN(getUltimoElemento(getListaPost sn))(getListaPost sn))))
          '()
          )
      '()
      )
  )
(define (remove-elemSN elem lista)
  (if (null? lista)
      lista
      (if (eqv? elem (car lista))
          (remove-elemSN elem (cdr lista))
          (cons (car lista)(remove-elemSN elem (cdr lista))))))

;Funcion que cambia la lista de publicaciones de un tda social network
;dom: tda socialnetwork x lista tda post
;rec: tda sn
(define(setListaPostSN sn newListaPost)
  (if (socialnetwork? sn)
      (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(getListaUser sn)
                                newListaPost)
      '()
      )
  )
;Funcion que cambia la lista de usuarios de un tda social network
;dom: tda socialnetwork x lista tda user
;rec: tda sn
(define(setListaUser sn listaUser)
  (if (socialnetwork? sn)
      (if (socialnetwork? (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)listaUser
                                (getListaPost sn)))
          (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)listaUser
                                (getListaPost sn))
          '()
          )
      '()
      ))
;Funcion que aplica una funcion a un usuario, Esa variable no es que sea un termine que varie, sino que puede variar si es string, numero ... etc
;dom: tda sn x user x procedure x elemento
;rec: tda sn
(define(aplicaSetUserPost sn user funcion variableaux)
  (if (socialnetwork? (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(setUser sn user funcion variableaux)
                                (getListaPost sn)))
          (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(setUser sn user funcion variableaux);aqui se puede comparar
                                (getListaPost sn))
          #f
          )
  )
;Funcion que compara listas,  las une dejando los valores iguales
;dom: lista x lista
;rec: lista
(define(comparaListas l1 l2 )
(if (or (null? l1)(null? l2))
    null
   (if (equal? (car l1)(car l2))
      (cons (car l1) (comparaListas (cdr l1) (cdr l2)))
      (cons (append (car l1) (car l2))(comparaListas (cdr l1)(cdr l2))))))
;(comparaListas '("chilo" "123" ((1 2 3)) '() (1 2 2021) "offline") '("chilo" "123" ((1 5 3)) '() (1 2 2021) "offline"))

;Funcion que une las listas de una matriz
;dom: matriz
;rec: lista
(define(appendListas matriz)
  (if(null? matriz)
     '()
     (append (car matriz)(appendListas (cdr matriz)))))


;Funcion que modifica al usuario con la funcion entrante "funcion"
;dom: tda sn x tda user x procedure x elemento
;rec: 
(define(setUser sn user funcion variableaux)
  (if (and (socialnetwork? sn)(validaUsuario user)(registradoSN? sn (getUser user)))
      (funcionAuxSetUser user (getListaUser sn) funcion variableaux)
      #f))
;sn (getUserOnlineSN sn) setEstado "offline" 

(define (funcionAuxSetUser user listaUser funcion variableaux)
  (if (null? listaUser)
      listaUser
      (if (equal? (getUser user) (car (car listaUser)))
          (cons (funcion user variableaux)(cdr listaUser))
          (cons (car listaUser)(funcionAuxSetUser user (cdr listaUser) funcion variableaux)))))
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
;descripción: Función que elimina los usuario no registrados en el socialnetwork
;dom: socialnetwork x lista string
;rec: lista user
(define (eliminaNoRegistrados sn listaUser)
  (if (socialnetwork? sn)
      (if (null? listaUser)
          listaUser
          (if(registradoSN? sn (car listaUser))
             (cons (car listaUser)(eliminaNoRegistrados sn (cdr listaUser)))
             (eliminaNoRegistrados sn (cdr listaUser))))
      #f)
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

;descripción: Funcion que en base al nombre del user obtiene el tda User
(define (getTdaUser sn user)
  (if (and (socialnetwork? sn)(string? user)(registradoSN? sn user))
      (auxGetTdaUser user (getListaUser sn))
     (display "1")
      
      ));valida si esta registrado, getslistaUser y se la paso a listaNombre y se la paso existe en lista

(define (aplicaGetTdaUser sn listaUser)
 (if(null? listaUser)
    null
  (if (and (socialnetwork? sn)(list? listaUser)(registradoSN? sn (car listaUser)))
      (cons (auxGetTdaUser (car listaUser) (getListaUser sn))(aplicaGetTdaUser sn (cdr listaUser)))
      (aplicaGetTdaUser sn (cdr listaUser)))))
#|(applicaGetTdaUser (register (register (socialnetwork "facebook" (date 25 10 2021) "encryptFn" "encryptFn")
 (date 1 3 21) "benja" "benja123")(date 1 3 21) "chilo" "benja123") '("benja" "chilo"))|#

(define (auxGetTdaUser user listaUser)
  (if(equal? user (car(car listaUser)))
     (car listaUser)
     (auxGetTdaUser user (cdr listaUser))
     )
  )
  

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


;Funcion que dado un socialnetwork retorna un tda usuario cuando esté este online
;
;
(define (getUserOnlineSN sn)
  (if (null? (getListaUser sn))
      '()
      (if (equal? "online" (getEstado (car (getListaUser sn))))
          (car (getListaUser sn))
          (getUserOnlineSN (setListaUser sn (cdr (getListaUser sn)))))))

;Funcion que obtiene un elemento de una lista dado un ID
;DOM: lista x entero
;REC: elementoLista
(define (getElemID lista id)
  (if (and (null? lista)(not(> id 0)))
      #f
      (if (= id 1)
          (car lista)
          (getElemID (cdr lista) (- id 1)))))

;Funcion que luego se aplicar una funcion ya sea post o lo que sea se vuelve Offline
(define(turnOff sn)
  (if (socialnetwork? sn)
      (setListaUser sn (aplicaTurnOff (getListaUser sn)));(setUser sn (getUserOnlineSN sn) setEstado "offline" )))
      #f))

(define(aplicaTurnOff  listaUser)
  
      (if (null? listaUser)
          listaUser
          (if (eqv? (getEstado(car listaUser))"online")
              (cons (setEstado (car listaUser) "offline")(cdr listaUser))
              (cons (car listaUser)(aplicaTurnOff  (cdr listaUser)));ACAAA
      )
      )
  )

#|
(define(aplicaSetUserEstado sn user funcion variableaux)
  (if (socialnetwork? (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(setUser sn user funcion variableaux)))
                                (getListaPost sn)))
          (list (getNameSN sn) (getDateSN sn)(getFnEnc sn) (getFnDesc sn)(setUser sn user funcion variableaux)
                                (getListaPost sn))
          (display "CAMBIAR A COND")
          )
  )
(define (aplicaSetUserEstadoListaAmigos sn )
  (if(socialnetwork? sn)
     (if (equal? (getUserOnlineSn sn)(car (getListaUser sn)))
         (setUser sn (getUserOnlineSn sn) setEstado "offline")
         (cons (car (getListaUser sn))(aplicaSetUserEstadoListaAmigos)))))|#








     
;(appendLista(user "Benja" "123" '() '())(list(user "Benja" "123" '() '())(user "chilo" "123" '() '())(user "juli" "123" '() '())))
;compara string (string-ci=? "Que" "qUe")

  
;LISTA USER = (list(user "Benja" "123" '() '())(user "chilo" "123" '() '())(user "juli" "123" '() '()))
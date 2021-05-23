#lang racket

(provide (all-defined-out))
(define (date dd mm yyyy)
         (if (and (integer? dd)(integer? mm)(integer? yyyy)
                  (> dd 0)(> mm 0)(<= mm 12)(> yyyy 0)
                  (<=  dd (getDiasDelMes mm yyyy)))
             (list dd mm yyyy)
             null
             )
  )
;Funcion que selecciona el dia de la fecha
;DOM: TDA date
;REC: INT
(define (getDD date)
         (car date))

;Funcion que selecciona el mes de la fecha
;DOM: TDA date
;REC: INT
(define (getMM date)
         (cadr date))

;Funcion que selecciona el año de la fecha
;DOM: TDA date
;REC: INT
(define (getYY date)
         (caddr date))



(define (date? date)
        (if (and(list? date)(= 3(length date))(integer? (getDD date))(integer? (getMM date))(integer? (getYY date)))
            #t
            #f))


;descripción: función para determinar si un año es bisiesto
;dom: entero
;rec: boolean
(define (bisiesto? yyyy)
  (if (and (integer? yyyy) (not (= yyyy 0)))
      (or (= (remainder yyyy 400) 0)
              (and (= (remainder yyyy 4) 0) (not (= (remainder yyyy 100) 0))))
      #f
  )
)
;descripción: función para determinar los días de un mes
;dom: entero X entero
;rec: entero
(define (getDiasDelMes mm yyyy)
  (if (and (integer? mm) (integer? yyyy) (not (= yyyy 0))
           (> mm 0) (< mm 13))
           (if (or (= mm 1) (= mm 3) (= mm 5) (= mm 7) (= mm 8) (= mm 10) (= mm 12))
                31
                (if (= mm 2)
                    (if (bisiesto? yyyy)
                        29
                        28
                    )
                    30
                )
            )
           0
   )
 )


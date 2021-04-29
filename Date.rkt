#lang racket

(define (date dd mm yyyy)
         (list dd mm yyyy))
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



(define (isDate date)
        (if (or(not(number? (getDD date)))(not(<= (getDD date) 31)))
            #f
            (if (or (not (number? (getMM date)))(not (<= (getMM date)12)))
                #f
                (if (or (not (number? (getYY date)))(not (>= (getYY date)2021)))
                    #f
                    #t))))
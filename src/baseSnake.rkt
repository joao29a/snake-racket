#lang racket

(require 2htdp/image)

(provide (struct-out posn)
         (struct-out snake)
         make-linha
         make-campo
         make-snake
         centraliza
         LARGURA-PADRAO ALTURA-PADRAO
         TIMEOUT-PADRAO
         DIRECAO-PADRAO
         COMEU-PADRAO
         BLANK
         CORES
         FRUTA-PADRAO)

(struct posn (lin col) #:transparent)

(struct snake (campo largura altura pos direcao timeout comeu? fruta)  #:transparent)

(define LARGURA-PADRAO 15)

(define ALTURA-PADRAO 15)

(define TIMEOUT-PADRAO 2)

(define DIRECAO-PADRAO "right")

(define COMEU-PADRAO #t)

(define BLANK (rectangle 0 0 "solid" "black"))

(define CORES (list "black"
                    "white"
                    "yellow"))

(define FRUTA-PADRAO (posn 0 0))

(define (make-linha n)
  (make-list n 0))

(define (make-campo largura altura)
  (make-list altura (make-linha largura)))

(define (centraliza snake-posn-list largura altura)
  (cond [(empty? snake-posn-list) empty]
        [else
         (define pos-lin (posn-lin (first snake-posn-list)))
         (define pos-col (posn-col (first snake-posn-list)))
         (define nova-pos (struct-copy posn (first snake-posn-list)
                                       (col (quotient (- largura 3) 2))
                                       (lin (quotient (- altura 3) 2))))
         (cons nova-pos (centraliza (rest snake-posn-list) largura altura))]))

(define (make-snake largura altura snake-posn direcao timeout comeu? fruta)
  (snake (make-campo largura altura)
         largura
         altura
         (centraliza snake-posn largura altura)
         direcao
         timeout
         comeu?
         fruta))

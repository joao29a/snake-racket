#lang racket

;; Fiz esse jogo na velocidade da luz e resultou que algumas 
;; funções estão ilegíveis. Posso melhorar o código futuramente, 
;; criar testes e documentar, mas só fiz mesmo pra brincar.

(require "baseSnake.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide make-snake-padrao
         lop-validas?
         lop-livres?
         fixa
         trata-tecla
         trata-tick
         desenha
         desenhar-campo
         mover-direita
         mover-esquerda
         mover-baixo
         mover-cima)

(define (make-snake-padrao)
  (make-snake LARGURA-PADRAO ALTURA-PADRAO (criar-snake) DIRECAO-PADRAO TIMEOUT-PADRAO
              COMEU-PADRAO FRUTA-PADRAO))

(define (trata-tecla jogo tecla) 
  (if (key=? tecla (snake-direcao jogo))
      jogo
      (mover tecla jogo)))

(define (direcao-oposta? snake-direcao ir-para)
  (or (or (and (equal? snake-direcao "right")
               (equal? ir-para "left"))
          (and (equal? snake-direcao "left")
               (equal? ir-para "right")))
      (or (and (equal? snake-direcao "up")
               (equal? ir-para "down"))
          (and (equal? snake-direcao "down")
               (equal? ir-para "up")))))

(define (mover ir-para jogo)
  (if (direcao-oposta? (snake-direcao jogo) ir-para)
      jogo
      (mover-snake ir-para (struct-copy snake jogo (direcao ir-para)))))

(define (mover-snake direcao jogo)
  (cond [(equal? direcao "right") (mover-direita jogo)]
        [(equal? direcao "left") (mover-esquerda jogo)]
        [(equal? direcao "up") (mover-cima jogo)]
        [(equal? direcao "down") (mover-baixo jogo)]
        [else jogo]))

(define (mover-direita jogo)
  (mover-horizontal add1 jogo))

(define (mover-esquerda jogo)
  (mover-horizontal sub1 jogo))

(define (mover-horizontal direcao-op jogo)
  (define snake-head (first (snake-pos jogo)))
  (define nova-pos (struct-copy posn snake-head (col (direcao-op (posn-col snake-head)))))
  (criar-snake-nova-pos nova-pos jogo))

(define (mover-cima jogo)
  (mover-vertical sub1 jogo))

(define (mover-baixo jogo)
  (mover-vertical add1 jogo))

(define (mover-vertical direcao-op jogo)
  (define snake-head (first (snake-pos jogo)))
  (define nova-pos (struct-copy posn snake-head (lin (direcao-op (posn-lin snake-head)))))
  (criar-snake-nova-pos nova-pos jogo))

(define (criar-snake-nova-pos nova-pos jogo)
  (define snake-sem-cauda (take (snake-pos jogo) (- (length (snake-pos jogo)) 1)))
  (define snake-nova-pos (cons nova-pos snake-sem-cauda))
  (define novo-campo (adicionar-valor-no-campo (snake-campo jogo) (drop (snake-pos jogo) 
                                                                        (- (length (snake-pos jogo)) 1)) 0))
  (if (and (lop-validas? (list nova-pos) (snake-largura jogo) (snake-altura jogo))
           (lop-comeu? nova-pos novo-campo))
      (struct-copy snake jogo 
                   (comeu? #t)
                   (pos (cons nova-pos (snake-pos jogo)))
                   (campo (adicionar-valor-no-campo novo-campo (list nova-pos) 0)))
      (if (and (lop-validas? (list nova-pos) (snake-largura jogo) (snake-altura jogo))  
               (lop-livres? (list (first snake-nova-pos)) novo-campo))
          (struct-copy snake jogo (pos snake-nova-pos)
                       (campo novo-campo))
          (make-snake-padrao))))

(define (lop-comeu? nova-pos campo)
  (if (= (list-ref (list-ref campo (posn-lin nova-pos)) (posn-col nova-pos)) 2)
      #t
      #f))

(define (lop-validas? lp largura altura)
  (cond [(empty? lp) #t]
        [else 
         (define lin (posn-lin (first lp)))
         (define col (posn-col (first lp)))
         (if (and 
              (and (< lin altura) (>= lin 0))
              (and (< col largura) (>= col 0)))
             (lop-validas? (rest lp) largura altura)
             #f)]))

(define (lop-livres? lp campo)
  (cond [(empty? lp) #t]
        [else
         (define lin (posn-lin (first lp)))
         (define col (posn-col (first lp)))
         (if (= (list-ref (list-ref campo lin) col) 0)
             (lop-livres? (rest lp) campo)
             #f)]))

(define (calc-novo-timeout timeout) 
  (if (>= timeout TIMEOUT-PADRAO)
      0
      (add1 timeout)))

(define (trata-tick jogo)
  (define timeout (snake-timeout jogo))
  (define newTimeout (calc-novo-timeout timeout))
  (define novo-jogo (struct-copy snake jogo
                                 (timeout newTimeout)))
  (define (verificar-timeout jogo) 
    (if (= (snake-timeout novo-jogo) 0)
        (fixa (mover (snake-direcao jogo) jogo))
        (fixa jogo)))
  (let ([frutaPos (posicionar-fruta jogo)])
    (if (snake-comeu? novo-jogo)
        (verificar-timeout (struct-copy snake novo-jogo
                                        (fruta frutaPos)
                                        (comeu? #f)
                                        (campo (adicionar-valor-no-campo (snake-campo jogo)
                                                                         (list frutaPos) 2))))
        (verificar-timeout novo-jogo))))

(define (posicionar-fruta jogo)
  (define fruta-pos (posn (random (snake-altura jogo))
                          (random (snake-largura jogo))))
  (if (lop-livres? (list fruta-pos) (snake-campo jogo))
      fruta-pos
      (posicionar-fruta jogo)))

(define (desenha jogo)
  (desenhar-campo (snake-campo jogo) 
                  (snake-largura jogo)
                  (snake-altura jogo)))

(define (desenhar-campo campo largura altura)
  (cond [(empty? campo) BLANK]
        [else (above (desenhar-linha (first campo) largura altura)
                     (desenhar-campo (rest campo) largura altura))]))

(define (desenhar-linha linha largura altura)
  (cond [(empty? linha) BLANK]
        [else (beside (rectangle largura altura "solid" (list-ref CORES (first linha)))
                      (desenhar-linha (rest linha) largura altura))]))

(define (adicionar-valor-no-campo campo list-posn cor)
  (cond [(empty? list-posn) campo]
        [else
         (define lin (posn-lin (first list-posn)))
         (define col (posn-col (first list-posn)))
         (define first-campo (take campo lin))
         (define rest-campo (drop campo lin))
         (define first-line (take (first rest-campo) col))
         (define rest-line (drop (first rest-campo) col))
         (define novo-campo (append first-campo
                                    (cons (append first-line
                                                  (cons cor (rest rest-line)))
                                          (rest rest-campo))))
         (adicionar-valor-no-campo novo-campo (rest list-posn) cor)]))

(define (fixa jogo)
  (struct-copy snake jogo (campo 
                           (adicionar-valor-no-campo (snake-campo jogo)
                                                     (snake-pos jogo)
                                                     1))))
(define (criar-snake)
  (list (posn 0 0) (posn 0 0)))

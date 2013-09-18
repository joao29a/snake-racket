#lang racket

(require "snake.rkt")
(require 2htdp/universe)

(big-bang (make-snake-padrao)
          (on-key trata-tecla)
          (on-tick trata-tick)
          (on-draw desenha))
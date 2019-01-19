#lang racket

(require "cpu.rkt")
(require "RAM.rkt")

(define accum
               ;(cpu (ram-load ram 0
                     '(6     ; [0]: first instruction
                       1     ; [1]: literal 1
                       1     ; [2]: literal 1
                       7     ; [3]: literal 7
                       0     ; [4]: n
                       0     ; [5]: acc
                       80400 ; input n
                       40004 ; skip next if n = 0
                       10002 ; skip next 1 instructions
                       90000 ; halt
                       10504 ; acc = acc + n
                       70005 ; out acc
                       20003 ; go back 7 (from next instr)
                       )) ;0))

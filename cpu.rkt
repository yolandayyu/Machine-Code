#lang racket

(require "RAM.rkt")
(provide cpu)
(provide ram-load)

;; Internal CPU helper functions

(define (cpu-add1 ram target-addr)            ; [target-addr] = [target-addr] + 1
  (ram-store ram target-addr (add1 (ram-fetch ram target-addr))))
(define (cpu-add ram target-addr source-addr) ; [target-addr] = [target-adr]+[source-addr]
  (ram-store ram target-addr (+ (ram-fetch ram target-addr)(ram-fetch ram source-addr))))
(define (cpu-sub ram target-addr source-addr) ; [target-addr] = [target-adr]-[source-addr]
  (ram-store ram target-addr (- (ram-fetch ram target-addr)(ram-fetch ram source-addr))))
(define (cpu-add1ifzero ram target-addr source-addr) ; if [source-addr] = 0 then [target-addr] = [target-addr] + 1
  (if (zero? (ram-fetch ram source-addr))(cpu-add1 ram target-addr)ram))
(define (cpu-copy ram target-addr source-addr) ; [target-addr] = [source-addr]
  (ram-store ram target-addr (ram-fetch ram source-addr)))
(define (cpu-fetch ram target-addr source-ptr-addr) ; [target-addr] = [[source-addr]]
  (cpu-copy ram target-addr (ram-fetch ram source-ptr-addr)))
(define (cpu-store ram target-ptr-addr source-addr) ; [[target-addr]] = [[source-addr]]
  (cpu-copy ram (ram-fetch ram target-ptr-addr) source-addr))
(define (cpu-output ram source-addr) ;           output = [source-addr]
  (display (ram-fetch ram source-addr))(newline)
  ram)
(define (cpu-input ram target-addr) ;            [target-addr] = input
  (ram-store ram target-addr (read)))

;; Loader -- put program and data into RAM

(define (ram-load ram load-addr lst);  [target-addr] = (car list), [target-addr+1] = (cadr list), ...
  (cond
    [(empty? lst) ram]
    [true
     (ram-load (ram-store ram load-addr (car lst)) (add1 load-addr) (cdr lst))]))

;; CPU fetch-eval cycle

(define (cpu ram instruction-ptr-addr)
 (define (cycle ram)
  (define instruction-pointer (ram-fetch ram instruction-ptr-addr))
  (define instruction (ram-fetch ram instruction-pointer))
  (define r1 (cpu-add1 ram instruction-ptr-addr))
  (define operation-code (quotient instruction 10000))
  (define target-addr (modulo (quotient instruction 100) 100))
  (define source-addr (modulo instruction 100))
  ;(printf "op ~a target ~a source ~a\n" operation-code target-addr source-addr)
  (cond
    [(= 0 operation-code)(cycle(cpu-add1 r1 target-addr))]
    [(= 1 operation-code)(cycle(cpu-add r1 target-addr source-addr))]
    [(= 2 operation-code)(cycle(cpu-sub r1 target-addr source-addr))]
    [(= 3 operation-code)(cycle(cpu-copy r1 target-addr source-addr))]
    [(= 4 operation-code)(cycle(cpu-add1ifzero r1 target-addr source-addr))]
    [(= 5 operation-code)(cycle(cpu-fetch r1 target-addr source-addr))]
    [(= 6 operation-code)(cycle(cpu-store r1 target-addr source-addr))]
    [(= 7 operation-code)(cycle(cpu-output r1 source-addr))]
    [(= 8 operation-code)(cycle(cpu-input r1 target-addr))]
    [(= 9 operation-code) r1])) ;; halt and return RAM state
  (cycle ram))

;; Sample Machine Language program
;; ouputs 84, 42, then echoes (numeric) input until 0 is input

;; Format of machine-language program
;; adjacent words [a][a+1] ... where a is load-addr
;;    [a]:  the first machine instruction is at load-addr+[a]
;;          [a] is called the instruction pointer or program counter
;;    [a+1]..[a+[a]-1]: programmer-defined
;;    [a+[a]].. machine-language program instruction words
;; Each instruction word is a 5-digit number: ottss
;;  o - opcode
;;  t - target
;;  s - source
;;  instruction meaning
;;  0ttss       [tt] = [tt] + 1
;;  1ttss       [tt] = [tt] + [ss]
;;  2ttss       [tt] = [tt] - [ss]
;;  3ttss       [tt] = [ss]
;;  4ttss       [tt] = [tt] + 1, if [ss] = 0
;;  5ttss       [tt] = [[ss]]
;;  6ttss       [[tt]] = [ss]
;;  7ttss       display [ss]
;;  8ttss       read [tt]
;;  9ttss       core dump and halt

#|(core-dump (cpu (ram-load ram 0
                     '(2     ; [0]: first instruction
                       0     ; [2]: n
                       80100 ; input n
                       39901 ; copy n to [99]
                       90000 ; halt
                       )) 0))
  
(define r1 (ram-load ram 0
   '(2     ; first instruction
     42    ; literal 42 -- not an instruction
     60101 ; [[1]] = [1]
     10101 ; [1] = [1] + [1]
     70001 ; out [1]
     70042 ; out [42]
     34400 ; [44] = [0] (save instruction pointer)
     85500 ; in [55]
     70055 ; out [55]
     40055 ; [0] = [0]+1, if [55] = 0
     30044 ; [0] = [44] (reset instruction pointer; go back)
     90000 ; halt
     )))|#

;(cpu r1 0)

;; Machine language programs to sum numbers from 1 to n
#|'(define r (cpu (ram-load ram 0
                     '(6     ; [0]: first instruction
                       1     ; [1]: literal 1
                       2     ; [2]: literal 2
                       7     ; [3]: literal 7
                       0     ; [4]: n
                       0     ; [5]: acc
                       80400 ; input n
                       40004 ; skip next if n = 0
                       10002 ; skip next 2 instructions
                       70005 ; out acc
                       90000 ; halt
                       10504 ; acc = acc + n
                       20401 ; n = n - 1
                       20003 ; go back 7 (from next instr)
                       )) 0))
|#


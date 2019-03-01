.386
ASSUME CS:code, DS:data, SS:stack

stack SEGMENT STACK
  DW 64 dup (0)
stack ends

data SEGMENT para PUBLIC 'DATA'
  IN1 DB 11001100B
  IN2 DB 10101010B
  IN3 DB 00011101B
  IN4 DB 11110000B
  IN5 DB 11001100B
  IN6 DB 00100101B
  ANS DB ?
  ANS1 DB ?
  ANS2 DB ?
data ends

code SEGMENT USE16 PARA PUBLIC 'CODE'
start:
  MOV AX, data
  MOV DS, AX

  ; logical operation
  MOV AL, IN1
  MOV AH, IN2
  MOV BH, AH

  NOT AH
  OR AL, AH
  MOV ANS1, AL
  ; ----

  MOV AL, IN3
  NOT AL
  MOV AH, IN4
  XOR AL, AH
  MOV ANS2, AL
  ; ----

  MOV AL, IN5
  OR BH, AL
  ; ----

  MOV AH, IN6
  AND AL, AH
  ; ----

  AND AL, ANS2
  ; ----

  XOR AL, BH
  ; ----

  OR AL, ANS1
  MOV ANS, AL

  ; (exit) terminate process/program
  MOV AH, 4Ch
  INT 21h
code ENDS
END start

.386
ASSUME CS:code, DS:data, SS:stack

stack SEGMENT STACK
    DW 64 dup (0)
stack ends

data SEGMENT para PUBLIC 'DATA'
    IN1 DB 01001100B
    IN2 DB 00110011B
    IN3 DB 10101010B
    IN4 DB 00011100B
    IN5 DB 11001111B
    ANS DB ?
data ends

code SEGMENT USE16 PARA PUBLIC 'CODE'
start:
    MOV AX, data
    MOV DS, AX

    ; logical operation
    MOV AL, IN1
    MOV AH, IN2
    NOT AH
    AND AL, AH
    NOT AL
    ; ----
    MOV BL, IN3
    NOT BL
    MOV BH, IN4
    AND BL, BH
    NOT BL
    ; ----
    OR BH, IN5
    XOR BL, BH
    OR AL, BL
    MOV ANS, AL

    ; (exit) terminate process/program
    MOV AH, 4Ch
    INT 21h
code ENDS
END start
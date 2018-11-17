.386

mycode SEGMENT USE16 PARA PUBLIC 'CODE'
ASSUME CS:mycode, DS:mydata

start:
    MOV AX, mydata
    MOV DS, AX
    SUB DX, DX

    MOV AX, NUM1
    DIV NUM2
    MOV ANS, AX

    MOV AH, 4Ch
    INT 21h
    mycode ENDS

    mydata SEGMENT para PUBLIC 'DATA'
        NUM1 DW 0Fh
        NUM2 DW 05h
        ANS DW ?
    mydata ends

    stack SEGMENT STACK
        DW 64 dup (0)
    stack ends

END start
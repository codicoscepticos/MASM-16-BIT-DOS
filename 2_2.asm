.386

code SEGMENT USE16 PARA PUBLIC 'CODE'
ASSUME CS:code, DS:data, SS:stack

start:
    MOV AX, data
    MOV DS, AX
    MOV DX, OFFSET string

    MOV AH, 09h
    INT 21h
    MOV AH, 4Ch
    INT 21h
    code ENDS

    data SEGMENT para PUBLIC
        string DB "Name Surname", 13, 10, "$"
    data ends

    stack SEGMENT STACK
        DW 64 dup (0)
    stack ends

END start
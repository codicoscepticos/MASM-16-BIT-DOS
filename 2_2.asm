.386
ASSUME CS:code, DS:data, SS:stack

stack SEGMENT STACK
  DW 64 dup (0)
stack ends

data SEGMENT para PUBLIC
  string DB "FirstName LastName", 13, 10, "$"
data ends

code SEGMENT USE16 PARA PUBLIC 'CODE'
start:
  MOV AX, data
  MOV DS, AX

  MOV DX, OFFSET string
  MOV AH, 09h
  INT 21h
  
  MOV AH, 4Ch
  INT 21h
code ENDS
END start

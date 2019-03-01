.386
ASSUME CS:mycode, DS:mydata

stack SEGMENT STACK
  DW 64 dup (0)
stack ends

mydata SEGMENT para PUBLIC 'DATA'
  NUM1 DW 0Fh
  NUM2 DW 05h
  ANS DW ?
mydata ends

mycode SEGMENT USE16 PARA PUBLIC 'CODE'
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
END start

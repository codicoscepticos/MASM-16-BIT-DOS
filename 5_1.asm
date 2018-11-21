.486

assume cs:code, ds:data, ss:stack

disp MACRO x
    push ax
    push dx

    mov ah, 09h
    mov dx, offset x
    int 21h

    pop dx
    pop ax
ENDM

; Constants
vidmem 0b800h
color equ 7
scrw equ 80*25
eom equ 0
dollar equ '$'

; Segments
stack segment use16
    db 256 dup(' ')
stack

data segment use16
    buffer db 4 dup(0)
    db dollar
    new_line db 10, 13, 13
    db dollar
    space db ' '
    db dollar
    line_2 dw 160d
    line_3 dw 320d
    msg db 'Doste ta stoixeia tou 3*3 pinaka:'
    db eom
    msg1 db '1) Gia na dosete kainourgia stoixeia ston pinaka.'
    db eom
    msg2 db '0) Gia exodo apo to programma.'
    db eom
    buf1 db 257 dup(?)
    db eom
    array dw 18 dup(0) ; 3*3=9*2=18
    db eom
data

code segment use16
start:
    main proc far
        push ds
        mov ax, 0
        push ax

        mov ax, data
        mov ds, ax

        mov ax, vidmem
        mov es, ax

        begin:
        call cls
        mov di, 0

        again:

        
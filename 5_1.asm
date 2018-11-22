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
vidmem equ 0b800h
cls_color equ 7
scrw equ 80*25
eom equ 0
dollar equ '$'

; Segments
stack segment use16 para stack
  db 256 dup(' ')
stack ends

data segment use16
  buffer db 4 dup(0)
  db dollar
  new_line db 10, 13, 13
  db dollar
  space db ' '
  db dollar

  line_1 dw 0d
  line_2 dw 160d
  line_3 dw 320d
  line_24 dw 3680d

  msg db 'Doste ta stoixeia tou 3*3 pinaka:'
  db eom
  msg0 db '0) Gia exodo apo to programma.'
  db eom
  msg1 db '1) Gia na dosete kainourgia stoixeia ston pinaka.'
  db eom

  buf1 db 257 dup(?)
  db eom
  array dw 18 dup(0) ; 3*3=9*2=18
  db eom
data ends

code segment use16 para public 'code'
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
    push di
    call cls

    lea si, msg
    mov di, line_24
    print_prompt:
      movsb
      inc di
      cmp byte ptr[si], eom
    jne print_prompt

    pop di
    mov cx, 10
    mov bx, 0

    readn1:
      mov ah, 1 ; read the keyboard
      int 21h

      mov ah, 0
      cmp al, '0'
      jb readn2
      cmp al, '9'
      ja readn2

      sub al, '0' ; convert number (of type string) to hex format (e.g.: ('1'=31h) - ('0'=30h) = 1h)

      push ax
      mov ax, bx
      mul cx
      mov bx, ax
      pop ax

      mov ah, 0
      add bx, ax
    jmp readn1

    readn2:
    mov ax, bx
    mov array[di], ax
    add di, 2
    cmp di, 18
    je break
    jmp again

    break:
    call cls
    mov bx, 1
    mov si, 0

    back:
      mov ax, array[si]
      mov dx, 0
      mov cx, 1000d
      div cx ; dx-ax / cx => dx = remainder, ax = quotient
      push ax

      ; ----
      push ax
      call hex2ascii
      pop ax

      disp buffer[3]

      xchg dx, ax
      ; ----

      mov cl, 100d
      div cl ; ax / 100 => ah = remainder, al = quotient

      mov dx, 0
      mov dl, al
      push ax

      ; ----
      push ax
      call hex2ascii
      pop ax

      disp buffer[3]

      xchg al, ah
      ; ----

      mov ah, 0
      mov cl, 10d
      div cl ; ax / 100 => ah = remainder, al = quotient

      mov dx, 0
      mov dl, al
      push ax

      ; ----
      push dx
      call hex2ascii
      pop ax

      disp buffer[3]

      xchg al, ah
      ; ----

      mov ah, 0
      push ax

      push ax
      call hex2ascii
      pop ax

      disp buffer[3]
      ; ---- ----

      disp space
      add si, 2

      inc bx
      cmp bx, 4d
    jne back
      mov bx, 1
      disp new_line
      cmp si, 18d
    jne back

    ; MENU
    ; Print 0th message; which tells how to exit.
    lea si, msg0
    mov di, line_1
    l_msg0:
      movsb
      inc di
      cmp byte ptr[si], eom
    jne l_msg0

    ; Print 1st message; which tells how to give new numbers.
    lea si, msg1
    mov di, line_2
    l_msg1:
      movsb
      inc di
      cmp byte ptr[si], eom
    jne l_msg1

    kbd:
      mov ah, 6 ; direct console read/write
      mov dl, 0ffh; ; reads the console, sets AL to typed ASCII character
      int 21h
    je kbd ; if ZF is set, i.e. no key was typed, then jump
      cmp al, '0'
      je l_exit
      cmp al, '1'
      je begin
    jmp kbd

    l_exit:
    ret
  main endp

  ; Subroutines
  cls proc near
		push cx
		mov di, 0

		mov al, ' '
		mov ah, cls_color

		mov cx, scrw
		rep stosw ; es:di <- ax, di <- di+2

		pop cx
		ret
	cls endp

  HEX2ASCII PROC NEAR
    ; Converts a word variable to ASCII
    ; Writes the results in the BUFFER (4 bytes) variable
    ; PARAMETERS
    ; gets a word variable from the stack
    push bp
    mov bp, sp
    mov ax, [bp+4] ; take input variable
    push cx
    mov cx, 4
    mov bp, cx
    H2A1:
    push ax
    and al, 0Fh
    add al, 30h
    cmp al, '9'
    jbe H2A2
    add al, 7h
    H2A2:
    dec cx
    mov bp, cx
    mov buffer[bp], al
    pop ax
    ror ax, 4
    jnz H2A1
    pop cx
    pop bp
    ret 2
  HEX2ASCII ENDP
  
code ends
end start
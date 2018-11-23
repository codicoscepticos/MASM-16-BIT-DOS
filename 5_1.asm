.486
assume cs:code, ds:data, ss:stack

disp MACRO x
  push ax
  push dx

  mov ah, 09h ; display a character string (must end with an ASCII $ (24H))
  mov dx, offset x ; offset of x from segment (where data is located), dx refers to the address of the character string
  int 21h

  pop dx
  pop ax
ENDM

; Constants
vidmem equ 0b800h
scrw equ 80*25
cls_color equ 7

dollar equ '$'
eom equ 0

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
  line_25 dw 3840d

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

    read_nums:
    mov di, 0 ; counter of elements/numbers; mainly, the destination index of the array where numbers are stored

    read_nums_loop:
    call cls

    push di
    lea si, msg
    mov di, line_24
    print_prompt:
      movsb
      inc di
      cmp byte ptr[si], eom
    jne print_prompt
    pop di

    mov cx, 10  ; multiplication factor; later, by multiplying ax each time, the whole number
                ; (temporarily referred by ax) is factored from units to thousands
    mov bx, 0   ; actual referrer of whole number; via addition with ax it forms the whole number (in hex format)

    read_num:
      mov ah, 1 ; read the keyboard, stores string input (a single char) to al, echoes/displays character on screen
      int 21h   ; (it doesn't wait for pressing ENTER, thus when you press a key, it immediately takes that as input and continues)
      mov ah, 0 ; ah is set to 0 (from 1), because later ax is used to construct (via addition with bx) the typed number

      cmp al, '0'
      jb store_read_num
      cmp al, '9'
      ja store_read_num
      ; if ENTER is pressed, which is equal to 0Dh, it will jump, too

      sub al, '0' ; convert ASCII number (of type string) to hex format (e.g.: ('1'=31h) - ('0'=30h) = 1h)

      push ax
      mov ax, bx  ; refers bx to ax, to
      mul cx      ; multiply ax with cx, and store result to ax
      mov bx, ax  ; ax is referred to bx which is the actual number holder
                  ; (here, ax is used as a temp register,
      pop ax      ; while its actual value is retained in the stack)

      add bx, ax  ; the previously typed numbers, referred by bx, and constructed so they form a whole number,
                  ; where each typed number takes the right place in the whole via the above multiplication (mul cx),
                  ; are added with ax, which currently refers to the last typed number (as the current units)
    jmp read_num

    store_read_num:
    mov array[di], bx
    add di, 2   ; Each data location is 8 bit (i.e. 1 byte each), ax is 16 bit (2 bytes),
                ; thus it takes 2 locations to be saved, and that's why di is increased by 2.
    cmp di, 18  ; And that's why it checks for 9*2=18 (here it has nothing to do about color).
    je disp_nums
    jmp read_nums_loop

    disp_nums:
    call cls

    mov bx, 0 ; counter of how many numbers have been displayed in the current row
    mov si, 0 ; source index of which element/number to retrieve from array

    disp_nums_loop:
      mov ax, array[si]
      mov dx, 0
      mov cx, 1000d
      div cx ; dx-ax / cx => dx = remainder, ax = quotient

      ; ----
      push ax
      push ax ; for some reason 'hex2ascii' expects ax to be pushed twice
              ; (or to find the input variable from a specific location of the stack)...
      call hex2ascii
      pop ax

      disp buffer[3]  ; display the thousands

      xchg dx, ax
      ; ----

      mov cl, 100d
      div cl ; ax / 100 => ah = remainder, al = quotient

      mov dx, 0
      mov dl, al

      ; ----
      push ax
      push ax
      call hex2ascii
      pop ax

      disp buffer[3]  ; display the hundreds

      xchg al, ah
      ; ----

      mov ah, 0
      mov cl, 10d
      div cl ; ax / 100 => ah = remainder, al = quotient

      mov dx, 0
      mov dl, al

      ; ----
      push ax
      push dx
      call hex2ascii
      pop ax

      disp buffer[3]  ; display the tens

      xchg al, ah
      ; ----

      mov ah, 0

      ; ----
      push ax
      push ax
      call hex2ascii
      pop ax

      disp buffer[3]  ; display the units
      ; ---- ----

      disp space
      add si, 2 ; increase si by 2 to get the next whole number in the next iteration

      inc bx
      cmp bx, 3d  ; 3 is the limit of how many whole numbers should be displayed in the current row,
                  ; once bx reach this, the code continues below
    jne disp_nums_loop
      mov bx, 0
      disp new_line
      cmp si, 18d ; same situation with 'cmp di, 18' above
    jne disp_nums_loop

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
      je read_nums
    jmp kbd

    l_exit:
    ret
  main endp

  ; Subroutines
  cls proc near
    push cx
    push di
		mov di, 0

		mov al, ' '
		mov ah, cls_color

		mov cx, scrw
		rep stosw ; es:di <- ax, di <- di+2

    pop di
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
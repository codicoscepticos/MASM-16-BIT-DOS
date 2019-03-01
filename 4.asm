.486
assume cs:code, ds:data, ss:stack

; Constants
vidmem equ 0b800h
scrw equ 80*25
cls_color equ 2
eom equ 0
chars_to_move equ 200

; Segments
stack segment use16 para stack
  db 256 dup (' ')
stack ends

data segment use16 para public
  line dw 320d ; 3rd line
  color db 0d
  msg db 'FirstName.LastName'
  db eom
data ends

code segment use16 para public 'code'
start:
main proc far
  mov ax, data
  mov ds, ax

  mov ax, vidmem
  mov es, ax

  mov dx, 2 ; times to loop
  begin:
  mov cx, chars_to_move
  cmp dx, 0
  je p_exit

  loop1:
  call cls
  call print_msg
  call sleep55
  add line, 2
  inc color
  loop loop1

  mov cx, chars_to_move
  loop2:
  call cls
  call print_msg
  call sleep55
  sub line, 2
  inc color
  loop loop2

  dec dx
  jmp begin

  p_exit:
  mov ah, 4ch
  int 21h
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

print_msg proc near
  mov di, line
  mov ah, color
  push cx
  mov cx, 21d
  rep stosw
  pop cx

  lea si, msg
  mov di, line
  print_loop:
    movsb
    inc di
    cmp byte ptr[si], eom
  jne print_loop

  ret
print_msg endp

sleep55 proc near
  push es
  push ax
  push cx

  mov cx, 1d
  sleep110ms:
    mov ax, 40h
    mov es, ax
    mov ax, es:[6ch]
    RTC_MustChange:
      cmp ax, es:[6ch]
    je RTC_MustChange
  loop sleep110ms

  pop cx
  pop ax
  pop es
  ret
sleep55 endp

code ends
end start

.486
assume cs:code, ds:data, ss:stack

; Macros
print_text_on_cursor MACRO text ; text must end with '$'
  push ax
  push dx

  mov ah, 09h ; display a text string (must end with an ASCII $ (24H))
  mov dx, offset text ; offset of text from segment (where data is located), dx's value is the address of the text string
  int 21h

  pop dx
  pop ax
ENDM

print_text_at_pos MACRO text, pos ; text must end with '0'
  LOCAL l_pt

  push si
  push di

  lea si, text
  mov di, pos
  l_pt:
    movsb ; move string byte/data, es:di <- ds:si, di <- di+1, si <- si+1
    inc di
    cmp byte ptr[si], eom
  jne l_pt

  pop di
  pop si
ENDM

printHex2Ascii MACRO hex, digits_num
  LOCAL mulbx
  LOCAL print_loop
  LOCAL end_macro

  push bx

  mov ax, hex
  mov dx, 0

  mov cx, digits_num
  dec cx  ; cx is decreased by 1, to produce the right divisor (through bx)
  mov bx, 1
  mulbx:
    imul bx, 10
  loop mulbx
  mov cx, bx  ; cx now contains the divisor for the first division with dx-ax (the divident) below

  print_loop:
    mov dx, 0 ; We don't care about the quotient (which after the xchg is now on dx),
              ; and also it prevents problems with the division (div cx),
              ; because otherwise if dx is not 0, the dividend (dx-ax) would become
              ; a quite large and unwanted/wrong number, and the program will crash
              ; (especially when the divisor (cx) equals 1, the quotient (ax) would be
              ; equal to the dividend (dx-ax) and as it's obvious ax (16 bit) cannot
              ; store dx-ax (32 bit)).
    div cx ; dx-ax / cx => dx = remainder, ax = quotient

    push ax
    push ax ; ax is pushed twice, because the called subroutine below, when it returns,
            ; discards (with 'ret 2') what was just pushed onto the stack before it was called
            ; (actually, 'ret 2' increases the stack pointer by 2)
    call hex2ascii  ; 'call' first pushes the current address onto the stack, then does
                    ; an unconditional jump to the specified label (i.e. the name of the subroutine)
    pop ax

    print_text_on_cursor buffer[3]

    xchg dx, ax

    ; check condition:
    cmp cx, 1 ; when cx is 1, there is no meaning to perform the divisions anymore,
              ; thus the macro is considered finished
    je end_macro

    ; divide cx by 10:
    push ax
    push dx
    mov dx, 0
    mov ax, cx
    mov cx, 10
    div cx
    mov cx, ax
    pop dx
    pop ax
    ; ----
  jmp print_loop
  
  end_macro:
  pop bx
ENDM

; Constants
dollar equ '$'

; Segments
stack segment use16 para stack
  db 256 dup(' ')
stack ends

data segment use16
  array dw 9, 18, -118, 6, -125, 4, -9, 2, 1
  elements_num equ (($-array)/2)  ; $-array generates the length of the array
  ; (($-array)/2) calculates the number of elements:
  ; the length of the array is divided by 2, because each
  ; element takes 2 bytes in memory, and thus that calculation
  ; returns the number of elements.

  msg_in db 'Insert number: '
  db dollar
  new_line db 10, 13
  db dollar

  swap_count db 1
  
  sign dw dollar

  buffer db 4 dup(0)
  db dollar
data ends

code segment use16 para public 'code'
start:
main proc far
  ; set up stack for return
  push ds
  mov ax, 0
  push ax
  ; ----

  ; set DS register to data segment
  mov ax, data
  mov ds, ax

  print_text_on_cursor new_line
  call display_array
  print_text_on_cursor new_line
  print_text_on_cursor new_line

  ; ---- actual sorting starts here ----
  next_pass:
  mov al, swap_count  ; checks to see how many swaps have been made
                      ; in the current pass
  cmp al, 0 ; if no swaps, then the array is sorted,
  je done   ; and thus jump to 'done'
  mov swap_count, 0 ; if the array is no sorted yet, then reset swap_count to 0
  mov di, 0 ; index to array's element, starting from the first (position 0),
            ; also counter of how how many elements have been checked

  next_elem:
  shl di, 1 ; di=di*2
  mov ax, array[di]
  cmp ax, array[di+2] ; compare the first number with the next one
                      ; (each element/number takes 2 bytes in memory)
  jg swap ; if the next one is greater than the first, then jump to the
          ; label 'swap', where the swapping of the two numbers takes place,
          ; the two numbers exchange places in the array

  increment:
  shr di, 1 ; di=di/2, it neutralizes the effect from the command
            ; 'shl di, 1', found above (below 'next_elem')
  inc di
  mov al, elements_num
  and ax, 0fh ; mask: keeps only the low byte of ax (i.e. only al), and
              ; sets the high byte (i.e. ah) to 0
  cmp di, ax    ; checks if all elements have been checked
                ; (actually, the maximum value di can have is elements_num-1),              
  je next_pass  ; if so, jump to 'next_pass'
  jmp next_elem ; else, jump to 'next_elem'

  swap:
  mov bx, array[di+2]
  mov array[di+2], ax ; ax already has the value of 'array[di]' from 'next_elem'
  mov array[di], bx

  inc swap_count
  jmp increment

  ; ---- end of sorting ----
  done:
  call display_array
  print_text_on_cursor new_line
  print_text_on_cursor new_line
  ret ; return to DOS
main endp

; Subroutines
display_array proc near
  push di

  mov di, 0
  l_next_elem:
    push di

    shl di, 1 ; di=di*2
    mov ax, array[di]
    mov bx, ax
    ; check if it's a negative number:
    and ax, 0ff00h
    cmp ah, 0ffh
    je l_negative_number
    mov sign, dollar
    l_display:
    ; display the sign and the number
    print_text_on_cursor sign ; display the sign (just before the number),
                              ; (in case it's positive, display nothing)
    printHex2Ascii bx, 3  ; convert the absolute to ascii
    print_text_on_cursor new_line

    pop di

    inc di
    mov al, elements_num
    and ax, 0fh ; mask
    cmp di, ax
  jb l_next_elem

  pop di

  ret ; return to main procedure

  l_negative_number:
    mov sign, 242dh
    ; calculate the absolute value of the negative number:
    mov ax, array[di]
    and ax, 00ffh
    mov bx, 00ffh
    sub bx, ax
    add bx, 1
  jmp l_display
display_array endp

HEX2ASCII PROC NEAR
  ; Converts a word variable to ASCII
  ; Writes the results in the BUFFER (4 bytes) variable
  ; PARAMETERS
  ; gets a word variable from the stack
  push bp
  mov bp, sp
  mov ax, [bp+4]  ; take input variable
                  ; it takes the ax value that was pushed last before this subroutine was called
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
  ret 2 ; ... may optionally specify an immediate operand, by adding this constant to the stack pointer,
        ; they effectively remove any arguments that the calling program pushed on the stack before
        ; the execution of the call instruction.
        ; Effectively, in this case, it removes the data that was pushed onto the stack (i.e. 'push ax'),
        ; before this subroutine was called.
HEX2ASCII ENDP

code ends
end start
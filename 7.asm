.486
assume cs:code, ds:data, ss:stack

; Macros
m_print_text_on_cursor macro text_offset ; text must end with '$'
  push text_offset          ; [bp+4]
  call print_text_on_cursor ; [bp+2]
endm

m_print_text_at_pos macro text_offset, pos ; text must end with '0'
  push text_offset        ; [bp+6]
  push pos                ; [bp+4]
  call print_text_at_pos  ; [bp+2]
endm

m_print_result macro msg_offset, result
  push msg_offset   ; [bp+6]
  push result       ; [bp+4]
  call print_result ; [bp+2]
endm

m_set_cursor_pos macro row, col, pagenum
  push row            ; [bp+8]
  push col            ; [bp+6]
  push pagenum        ; [bp+4]
  call set_cursor_pos ; [bp+2]
endm

m_printHex2Ascii MACRO hex, digits_num
  push hex            ; [bp+6]
  push digits_num     ; [bp+4]
  call printHex2Ascii ; [bp+2] (push return address)
ENDM

; Constants
vidmem equ 0b800h
scrw equ 80*25
cls_color equ 07  ; background = 0 (black), foreground = 7 (white)

dollar equ '$'
eom equ 0
neg_sign equ 242dh	; 24 is the dollar ($) sign, 2d is the hyphen (-) character.
; They're placed according to the little-endian format, in which
; Intel x86 assembly is based on. This is used to display the negative sign.
; The dollar sign is included, because the macro 'm_print_text_on_cursor'
; terminates the displaying of text when it finds the dollar sign.

; size of array: 
array_rows equ 4
array_cols equ 4
elements_num equ array_rows*array_cols  ; 4*4=16
array_size equ elements_num*2 ; 16*2=32 (multiplied by 2, because each element
                              ; of the array takes 2 bytes in memory)

; Segments
stack segment use16 para stack
  db 256 dup(' ')
stack ends

data segment use16
  buffer db 4 dup(0)
  db dollar

  array dw array_size dup(0)
  db eom
  
  quotient dw 2 dup(0)

  new_line db 10, 13  ; 10=CR:Carriage Return, 13=LF:Line Feed (Windows endings)
  db dollar
  space db ' '
  db dollar
  comma db ','
  db dollar

  line_1 dw 0d
  line_2 dw 160d
  line_3 dw 320d
  line_4 dw 480d
  line_5 dw 640d
  line_24 dw 3680d  ; one line above from the last line on screen
  line_25 dw 3840d  ; last line on screen

  msg db 'Enter the elements of the ', array_rows+48, '*', array_cols+48, ' array:'
                                            ; 48 is added to the values to get
                                            ; their ASCII representation.
  db eom
  msg0 db '(0) Exit.'
  db eom
  msg1 db '(1) Enter new elements.'
  db eom
  msg2 db '(2) Print elements' sum.'
  db eom
  msg3 db '(3) Print elements' average.'
  db eom
  msg4 db '(4) Print power of two of average.'
  db eom

  msg_sum db 'Element's sum:'
  db eom
  msg_avg db 'Elements' average:'
  db eom
  msg_pow2_avg db 'Power of two of average:'
  db eom
  msg_gotoMenu db 'Press any key, to return...'
  db eom

  sign dw dollar    ; sign initialized as '$', because that way the display
                    ; macro will print nothing, and the absence of sign
                    ; is used for positive numbers
  leading_zero db 1 ; this is a flag

  sum dw 0
  db eom
  average dw 0
  db eom
  remainder dw 0
  db eom
  power2_average dw 0
  db eom
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

  ; set ES register to memory location
  ; (equals to vidmem = 0b800h) of extra segment
  mov ax, vidmem
  mov es, ax

  read_nums:
  mov di, 0 ; counter of elements/numbers (with step 2); mainly,
            ; the destination index of the array where numbers are stored

  read_nums_loop:
  call cls
  m_print_text_at_pos offset msg, line_24 ; 'Enter elements of X*X array:'

  mov cx, 10  ; multiplication factor; later, by multiplying ax each time,
              ; the whole number (temporarily as the value of ax) is factored
              ; from units to thousands
  mov bx, 0   ; actual container of whole number; via addition with ax
              ; it forms the whole number (in hex format)

  read_num:
    mov ah, 1 ; read the keyboard, stores string input (a single char) to al,
              ; echoes/displays character on screen
    int 21h   ; (it doesn't wait for pressing ENTER, thus when you press a key,
              ; it immediately takes that as input and continues)
    mov ah, 0 ; ah is set to 0 (from 1), because later ax is used to construct
              ; (via addition with bx) the typed number

    ; checks to see if typed number is out of range of accepted numbers (0-9),
    ; if so then jump
    cmp al, '0'
    jb store_read_num
    cmp al, '9'
    ja store_read_num
    ; if ENTER is pressed, which is equal to 0Dh, it will jump, too;
    ; these two jumps above are the only ways to break from this loop

    sub al, '0' ; convert ASCII number (of type string) to hex format
                ; (e.g.: ('1'=31h) - ('0'=30h) = 1h)
    push ax
    mov ax, bx  ; ax copies bx
    mul cx      ; multiply ax with cx, and store result to ax
    mov bx, ax  ; bx (which is the actual number container) copies ax
                ; (here, ax is used as a temp register,
    pop ax      ; while its actual value is retained in the stack)

    add bx, ax  ; the previously typed numbers, contained by bx, and constructed
                ; so they form a whole number, where each typed number takes the
                ; right place in the whole via the above multiplication (mul cx),
                ; are added with ax, which currently contains the last typed
                ; number (as the current units)
  jmp read_num

  store_read_num:
  mov array[di], bx
  add di, 2           ; Each data location is 8 bit (i.e. 1 byte each),
                      ; ax is 16 bit (2 bytes), thus it takes 2 locations to
                      ; be saved, and that's why di is increased by 2.
  cmp di, array_size  ; And that's why it checks for elements_num*2=array_size
                      ; (here it has nothing to do about color).
  je disp_nums
  jmp read_nums_loop

  disp_nums:
  call cls

  mov bx, 0 ; counter of how many numbers have been displayed in the current row
  mov si, 0 ; source index of which element/number to retrieve from array

  disp_nums_loop:
    m_printHex2Ascii array[si], 4       ; prints a whole number (of 4 digits)
                                        ; in ascii format
    m_print_text_on_cursor offset space

    add si, 2           ; Increase si by 2 to get the next
                        ; whole number in the next iteration.
    inc bx              ; A digit has been displayed, so bx is increased by 1
    cmp bx, array_rows  ; is the limit of how many whole numbers (of 4 digits)
                        ; should be displayed in the current row,
                        ; once bx reach this, the code continues below.
  jne disp_nums_loop
    mov bx, 0           ; reset bx, for the next iteration
    m_print_text_on_cursor offset new_line
    cmp si, array_size ; same situation with 'cmp bx, array_rows' above
  jne disp_nums_loop

  ; ---- PRECALCULATE EVERYTHING ----
  call calc_sum                   ; modifies variables: 'sum'
  call calc_average_and_remainder ; modifies variables: 'average', 'remainder'
  call calc_quotient              ; modifies variables: 'quotient[]'
  call calc_pow2_avg              ; modifies variables: 'power2_average'
  ; ---------------------------------

  ; MENU
  l_menu:
  ; Print 0th message; which tells how to exit.
  m_print_text_at_pos offset msg0, line_1
  ; Print 1st message; which tells how to give new numbers.
  m_print_text_at_pos offset msg1, line_2
  ; Print 2nd message; which tells how to print the sum.
  m_print_text_at_pos offset msg2, line_3
  ; Print 3rd message; which tells how to print the average.
  m_print_text_at_pos offset msg3, line_4
  ; Print 4th message; which tells how to print the average's square.
  m_print_text_at_pos offset msg4, line_5

  ; Keyboard input detection, for menu item selection:
  kbd:
    mov ah, 6 ; direct console read/write
    mov dl, 0ffh; ; reads the console, sets AL to typed ASCII character
    int 21h
  je kbd ; if ZF is set, i.e. no key was typed, then jump
    cmp al, '0'
    je l_exit
    cmp al, '1'
    je read_nums
    cmp al, '2'
    je l_print_sum
    cmp al, '3'
    je print_avg
    cmp al, '4'
    je print_pow2_avg
  jmp kbd

  l_print_sum:
    m_print_result offset msg_sum, sum
  jmp wait_for_key

  print_avg:
    m_print_result offset msg_avg, average
    m_print_text_on_cursor offset comma
    m_printHex2Ascii quotient[0], 1
    m_printHex2Ascii quotient[2], 1
  jmp wait_for_key

  print_pow2_avg:
    m_print_result offset msg_pow2_avg, power2_average
  ;jmp wait_for_key

  wait_for_key:
    call wait_key_press
    m_set_cursor_pos 24, 0, 0
  jmp disp_nums

  l_exit:
  ret ; return to DOS
main endp

; Subroutines
calc_sum proc near
    push ax
    push di
    push cx

    ; calculation of the sum
    mov ax, 0   ; register to contain the sum
                ; (it will be overflowed if the numbers are big)
    mov di, 0   ; index to array's element, starting from the first (position 0)
    mov cx, elements_num ; number of iterations, equal to element's number
    sum_loop:
      add ax, array[di]
      add di, 2 ; each number is 2 bytes long, so di is increased by 2
    loop sum_loop
    mov sum, ax	; copy the sum from the accumulator
                ; (ax) to its dedicated variable
    pop cx
    pop di
    pop ax

    ret
calc_sum endp

calc_average_and_remainder proc near
  push ax
  push cx
  push dx

  mov dx, 0
  mov ax, sum
  mov cx, elements_num
  div cx  ; dx:ax = dx:ax/cx        =>
          ; sum = sum/elements_num  =>
          ;                         => ax = average, dx = remainder
  mov average, ax
  mov remainder, dx

  pop dx
  pop cx
  pop ax
  ret
calc_average_and_remainder endp

calc_quotient proc near
  push ax
  push cx
  push dx
  push di

  ; reset quotient:
  mov quotient[0], 0
  mov quotient[2], 0

  mov di, 0
  mov dx, remainder
  l_cq:
    mov ax, dx
    mov cx, 10
    mul cx

    mov dx, 0
    mov cx, elements_num
    div cx  ; ax = average, dx = remainder
    mov quotient[di], ax ; digit of quotient

    cmp dx, 0
    je no_remainder

    add di, 2
    cmp di, 4
  jne l_cq

  no_remainder:
  pop di
  pop dx
  pop cx
  pop ax
  ret
calc_quotient endp

calc_pow2_avg proc near
  push ax
  
  mov ax, average
  mul ax
  mov power2_average, ax
  
  pop ax
  ret
calc_pow2_avg endp

print_text_on_cursor proc near  ; has macro, parameters: text_offset
  push bp ; [bp+0]
  mov bp, sp

  push ax
  push dx

  mov ah, 09h     ; display a text string (must end with an ASCII $ (24H))
  mov dx, [bp+4]  ; offset of text from segment (where data is located),
                  ; dx's value is the address of the text string
  int 21h

  pop dx
  pop ax

  pop bp
  ret 2
print_text_on_cursor endp

print_text_at_pos proc near  ; has macro, parameters: text_offset, pos
  push bp ; [bp+0]
  mov bp, sp

  push si
  push di

  mov si, [bp+6]  ; offset of text from segment
  mov di, [bp+4]  ; pos (position of text on screen)
  l_pt:
    movsb ; move string byte/data, es:di <- ds:si, di <- di+1, si <- si+1
    inc di
    cmp byte ptr[si], eom
  jne l_pt

  pop di
  pop si

  pop bp
  ret 4
print_text_at_pos endp

print_result proc near  ; has macro, parameters: msg_offset, result
  push bp ; [bp+0]
  mov bp, sp

  call cls  ; first clear the screen
            ; Display the message which tells that
            ; in the next line the result is displayed:
  m_print_text_at_pos [bp+6], line_24 ; [bp+6] = msg_offset
  ; print the result in that next line
  m_printHex2Ascii [bp+4], 5          ; [bp+4] = result
                                      ; displayed with 5 digits

  ; print message in the first line, which tells how to go back to the menu
  m_print_text_at_pos offset msg_gotoMenu, line_1

  pop bp
  ret 4 ; 2 parameters * 2 bytes = increase SP by 4
print_result endp

printHex2Ascii proc near  ; has macro, parameters: hex, digits_num
  push bp ; [bp+0]
  mov bp, sp

  push ax
  push bx
  push cx
  push dx

  ; The 1st parameter 'hex' is located 6 bytes above where the stack pointer
  ; currently points, and the 2nd parameter 'digits_num' 4 bytes.
  ; That's because previously has been pushed onto the stack (in descending
  ; order):
  ; [bp+2] => the return address (when the call to this subroutine was made)
  ; [bp+0]  => old base pointer
  mov ax, [bp+6]  ; hex
  mov dx, 0

  mov bx, 1       ; initial value of bx
  mov cx, [bp+4]  ; digits_num
  dec cx          ; cx is decreased by 1, to produce the right divisor
                  ; (using the loop 'mulbx').
  cmp cx, 0       ; if cx = 0, then
  je setcxbybx    ; don't multiply bx (but just set cx equals to bx), else
                  ; multiply bx by 10 each time (times equal to cx)
  mulbx:
    imul bx, 10
  loop mulbx

  setcxbybx:
  mov cx, bx  ; cx now contains the divisor for the first
              ; division with dx-ax (the divident) below.
  
  mov leading_zero, 1 ; If the first digit is a zero, it's considered as a
                      ; leading zero.
  print_loop:
    mov dx, 0
    ; We don't care about the quotient (which after the xchg is now on dx),
    ; and also it prevents problems with the division (div cx),
    ; because otherwise if dx is not 0, the dividend (dx-ax) would become
    ; a quite large and unwanted/wrong number, and the program will crash
    ; (especially when the divisor (cx) equals 1, the quotient (ax) would be
    ; equal to the dividend (dx-ax) and as it's obvious ax (16 bit) cannot
    ; store dx-ax (32 bit)).

    div cx    ; dx-ax / cx => dx = remainder, ax = quotient

    push ax
    push ax ; ax is pushed twice, because the called subroutine below,
            ; when it returns, discards (with 'ret 2') what was just pushed onto
            ; the stack before it was called (actually, 'ret 2' increases the
            ; stack pointer by 2)
    call hex2ascii  ; 'call' first pushes the current address onto the stack,
                    ; then does an unconditional jump to the specified label
                    ; (i.e. the name of the subroutine)
    pop ax

    ; ---- convert leading zeros (trim e.g. 0009 to    9, 0125 to  125 etc) ----
    push ax
    mov ah, buffer[3] ; Because we have defined buffer to contain elements
                      ; of 1 byte, we can't copy an element from buffer
                      ; to ax (which the latter is 2 bytes).
                      ; Therefore, we have to use ah (which is 1 byte) and
                      ; complies with the size of buffer's elements.
    cmp ah, '0'               ; Check if digit is 0, and
    jne l_leading_zero_false  ; if not, then digit is 1-9 and jump to
                              ; set the flag leading_zero to 0 (false) and
                              ; then just display the digit.
    cmp leading_zero, 1       ; (Else, digit is 0 and) check if it's a
                              ; leading zero, and
    jne l_display_digit       ; if not, then jump to just display it.
    cmp cx, 1                 ; (Else, digit is 0 and) check if it's the
                              ; last digit, and
    je l_display_digit        ; if it's, then jump to just display it.
                              ; Else, digit is 0 and it's a leading zero, so:
    mov buffer[3], 20h        ; We convert digit 0 to the space (20h) character.
    jmp l_display_digit       ; Jump to display it.
    
    l_leading_zero_false:
    mov leading_zero, 0 ; Zeroes that aren't leading, are going to be displayed.
    
    l_display_digit:
    m_print_text_on_cursor offset buffer[3]

    ; pseudocode:
    ; if (digit != 0) then
    ;   set leading_zero equal to 0/false
    ;   display the digit
    ; else if (digit == 0 AND leading_zero == 0/false) then
    ;   display the digit
    ; else  ; (digit == 0 AND leading_zero == 1/true)
    ;   convert the digit 0 to '$'
    ;   display the digit
    ; end
    pop ax
    ; ---- trimming ends here ----

    xchg dx, ax

    ; check condition:
    cmp cx, 1 ; when cx is 1, there is no meaning to perform the divisions
              ; anymore, thus the macro is considered finished
    je end_proc

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
  
  end_proc:
  ; restore... (popping in the reverse order compared to pushing)
  ; ...the values of the registers:
  pop dx
  pop cx
  pop bx
  pop ax
  ; ...the old base pointer:
  pop bp
  ret 4	; Probably not exactly in this order, but:
        ; * Pop from the stack the return address (and save it somewhere) and so
        ; increase SP (stack pointer) by 2.
        ; * Increase SP by 4, effectively removing the 2 parameters that were
        ; pushed onto the stack before the call of this subroutine.
        ; * Jump to the return address.
printHex2Ascii endp

cls proc near
  push ax
  push cx
  push di

  mov al, ' '
  mov ah, cls_color

  mov di, 0
  mov cx, scrw
  rep stosw ; es:di <- ax, di <- di+2

  pop di
  pop cx
  pop ax
  ret
cls endp

wait_key_press proc near
  push ax

  ; wait for any key press
  mov ah, 07h
  int 21h

  pop ax
  ret
wait_key_press endp

set_cursor_pos proc near  ; has macro, parameters: row, column, pagenum
  push bp ; [bp+0]
  mov bp, sp

  push ax
  push bx
  push dx

  ; set cursor position:
  mov ah, 2h       ; subfunction code
  mov dh, [bp+8]   ; row
  mov dl, [bp+6]   ; column
  mov bh, [bp+4]   ; display page number
  int 10h          ; https://en.wikipedia.org/wiki/INT_10H
  ; cursor is set back to the bottom-left position of the screen

  pop dx
  pop bx
  pop ax

  pop bp
  ret 6; 3 parameters * 2 bytes = increase SP by 6
set_cursor_pos endp

HEX2ASCII PROC NEAR
  ; Converts a word variable to ASCII
  ; Writes the results in the BUFFER (4 bytes) variable
  ; PARAMETERS
  ; gets a word variable from the stack
  push bp
  mov bp, sp
  mov ax, [bp+4]  ; take input variable
                  ; it takes the ax value that was pushed
                  ; last before this subroutine was called
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
  ret 2 ; ... may optionally specify an immediate operand,
        ; by adding this constant to the stack pointer,
        ; they effectively remove any arguments that the
        ; calling program pushed on the stack before
        ; the execution of the call instruction.
        ; Effectively, in this case, it removes the data
        ; that was pushed onto the stack (i.e. 'push ax'),
        ; before this subroutine was called.
HEX2ASCII ENDP

code ends
end start

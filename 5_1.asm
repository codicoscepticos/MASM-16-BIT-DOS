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
vidmem equ 0b800h
scrw equ 80*25
cls_color equ 07  ; background = 0 (black), foreground = 7 (white)

dollar equ '$'
eom equ 0

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
  db dollar ; a byte declared without a label, containing the value of dollar, its location is buffer + 4
            ; same thing with the other 'db dollar' declarations
  new_line db 10, 13, 13
  db dollar ; its location is new_line + 3
  space db ' '
  db dollar ; its location is space + 1

  line_1 dw 0d
  line_2 dw 160d
  line_3 dw 320d
  line_24 dw 3680d  ; one line above from the last line on screen
  line_25 dw 3840d  ; last line on screen

  msg db 'Type the elements of the ', array_rows+48, '*', array_cols+48, ' array:'
                                                ; 48 is added to the values to get their
                                                ; ASCII representation.
  db eom
  msg0 db '(0) Exit.'
  db eom
  msg1 db '(1) Give new elements.'
  db eom
  msg2 db '(2) Sum of elements.'
  db eom

  msg_sum db 'Sum of elements:'
  db eom
  msg_gotoMenu db 'Press any key, to return to the menu...'
  db eom

  array dw array_size dup(0)
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

    ; set ES register to memory location (equals to vidmem = 0b800h) of extra segment
    mov ax, vidmem
    mov es, ax

    read_nums:
    mov di, 0 ; counter of elements/numbers (with step 2); mainly, the destination index of the array where numbers are stored

    read_nums_loop:
    call cls
    print_text_at_pos msg, line_24 ; 'Type the elements of the X*X array:'

    mov cx, 10  ; multiplication factor; later, by multiplying ax each time, the whole number
                ; (temporarily as the value of ax) is factored from units to thousands
    mov bx, 0   ; actual container of whole number; via addition with ax it forms the whole number (in hex format)

    read_num:
      mov ah, 1 ; read the keyboard, stores string input (a single char) to al, echoes/displays character on screen
      int 21h   ; (it doesn't wait for pressing ENTER, thus when you press a key, it immediately takes that as input and continues)
      mov ah, 0 ; ah is set to 0 (from 1), because later ax is used to construct (via addition with bx) the typed number

      ; checks to see if typed number is out of range of accepted numbers (0-9),
      ; if so then jump
      cmp al, '0'
      jb store_read_num
      cmp al, '9'
      ja store_read_num
      ; if ENTER is pressed, which is equal to 0Dh, it will jump, too;
      ; these two jumps above are the only ways to break from this loop

      sub al, '0' ; convert ASCII number (of type string) to hex format (e.g.: ('1'=31h) - ('0'=30h) = 1h)

      push ax
      mov ax, bx  ; ax copies bx, to
      mul cx      ; multiply ax with cx, and store result to ax
      mov bx, ax  ; bx (which is the actual number container) copies ax
                  ; (here, ax is used as a temp register,
      pop ax      ; while its actual value is retained in the stack)

      add bx, ax  ; the previously typed numbers, contained by bx, and constructed so they form a whole number,
                  ; where each typed number takes the right place in the whole via the above multiplication (mul cx),
                  ; are added with ax, which currently contains the last typed number (as the current units)
    jmp read_num

    store_read_num:
    mov array[di], bx
    add di, 2 ; Each data location is 8 bit (i.e. 1 byte each), ax is 16 bit (2 bytes),
              ; thus it takes 2 locations to be saved, and that's why di is increased by 2.
    cmp di, array_size  ; And that's why it checks for elements_num*2=array_size (here it has nothing to do about color).
    je disp_nums
    jmp read_nums_loop

    disp_nums:
    call cls

    mov bx, 0 ; counter of how many numbers have been displayed in the current row
    mov si, 0 ; source index of which element/number to retrieve from array

    disp_nums_loop:
      printHex2Ascii array[si], 4  ; prints a whole number (of 4 digits) in ascii format

      print_text_on_cursor space

      add si, 2 ; increase si by 2 to get the next whole number in the next iteration

      inc bx      ; a digit has been displayed, so bx is increased by 1
      cmp bx, array_rows  ; is the limit of how many whole numbers (of 4 digits) should be displayed in the current row,
                          ; once bx reach this, the code continues below
    jne disp_nums_loop
      mov bx, 0   ; reset bx, for the next iteration
      print_text_on_cursor new_line
      cmp si, array_size ; same situation with 'cmp bx, array_rows' above
    jne disp_nums_loop

    ; MENU
    l_menu:
    ; Print 0th message; which tells how to exit.
    print_text_at_pos msg0, line_1
    ; Print 1st message; which tells how to give new numbers.
    print_text_at_pos msg1, line_2
    ; Print 2nd message; which tells how to print the sum.
    print_text_at_pos msg2, line_3

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
      je print_sum
    jmp kbd

    print_sum:
      push ax
      push di
      push cx

      ; calculation of the sum
      mov ax, 0 ; register to contain the sum (it will be overflowed if the numbers are big)
      mov di, 0 ; index to array's element, starting from the first (position 0)
      mov cx, elements_num ; number of iterations, equal to element's number
      sum_loop:
        add ax, array[di]
        add di, 2 ; each number is 2 bytes long, so di is increased by 2
      loop sum_loop

      ; display the message which tells that in the next line the sum is displayed
      call cls ; first clear the screen
      print_text_at_pos msg_sum, line_24
      ; print the sum in that next line
      printHex2Ascii ax, 5  ; sum is displayed with 5 digits

      ; print message in the first line, which tells how to go back to the menu
      print_text_at_pos msg_gotoMenu, line_1

      ; wait for any key press
      mov ah, 07h
      int 21h

      ; set cursor position:
      mov ah, 2h  ; subfunction code
      mov dh, 24  ; row
      mov dl, 0   ; column
      mov bh, 0   ; display page number
      int 10h     ; https://en.wikipedia.org/wiki/INT_10H
      ; cursor is set back to the bottom-left position of the screen

      pop cx
      pop di
      pop ax
    jmp disp_nums

    l_exit:
    ret
  main endp

  ; Subroutines
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
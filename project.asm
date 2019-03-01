.486
assume cs:code, ds:data, ss:stack

; Macros
  m_paint_area macro color, position, len
    push color    ; [bp+8]
    push position ; [bp+6]
    push len      ; [bp+4]
    call paint_area
  endm

  ; Strings must end with the dollar sign ('$').

  m_print macro string_offset, position, color
    push string_offset  ; [bp+8]
    push position       ; [bp+6]
    push color          ; [bp+4]
    call print          ; [bp+2]
  endm

  m_print_at_line macro string_offset, line_index, color
    push string_offset        ; [bp+8]
    push lines[line_index*2]  ; [bp+6] (convert line_index to position)
    push color                ; [bp+4]
    call print                ; [bp+2]
  endm

  m_print_at_cursor macro string_offset, color
    push string_offset  ; [bp+8]
    push cursor_pos     ; [bp+6] (this value is pushed without a parameter)
    push color          ; [bp+4]
    call print          ; [bp+2]
    add cursor_pos, 2   ; move cursor one place to the right (2 bytes per place)
  endm

  m_print_new_line macro
    call print_new_line
  endm

  print_new_lines macro number  ; number of new lines to print
    LOCAL l_loop
    push cx
    mov cx, number
    l_loop:
      m_print_new_line
    loop l_loop
    pop cx
  endm

  m_print_space macro
    m_print_at_cursor offset space, text_color
  endm

  m_find_string_length macro string_offset
    push string_offset      ; [bp+4]
    call find_string_length ; [bp+2]
  endm

  m_display_numbers macro row_index, rows_num
    push row_index        ; [bp+6]
    push rows_num         ; [bp+4]
    call display_numbers  ; [bp+2]
  endm

  m_display_row_info macro row_char_offset
    push row_char_offset  ; [bp+4]
    call display_row_info ; [bp+2]
  endm

  m_find_num_freq macro number
    push number         ; [bp+4]
    call find_num_freq  ; [bp+2]
  endm

  m_print_result macro msg_offset, result
    push msg_offset   ; [bp+6]
    push result       ; [bp+4]
    call print_result ; [bp+2]
  endm

  m_set_cursor_pos macro row, col
    push row            ; [bp+6]
    push col            ; [bp+4]
    call set_cursor_pos ; [bp+2]
  endm

  m_printHex2Ascii macro hex, digits_num
    push hex            ; [bp+6]
    push digits_num     ; [bp+4]
    call printHex2Ascii ; [bp+2] (push return address)
  endm
; ----

; Constants
  vidmem equ 0b800h
  scr_cols_num equ 80
  scr_rows_num equ 25
  scr_size equ scr_cols_num*scr_rows_num
  cls_color equ 07h     ; background = 0 (black), foreground = 7 (white)
  text_color equ 00e4h  ; background = E (yellow), foreground = 4 (red)
  dollar equ '$'
  ; size of array: 
  array_rows equ 6  ; The number of *rows*    of the array!
  array_cols equ 5  ; The number of *columns* of the array!
  elements_num equ array_rows*array_cols        ; 6*5=30
  element_byte_size equ 2
  array_size equ elements_num*element_byte_size ; 30*2=60
  last_element_index equ array_size - element_byte_size
; ----

; Segments
stack segment use16 para stack
  db 256 dup(' ')
stack ends

data segment use16
  author_name db '* * * * * FirstName LastName (AM) * * * * *'
  db dollar

  buffer db 4 dup(0)
  db dollar

  array dw array_size dup(0)
  db dollar
  
  quotient dw 2 dup(0)

  lines dw scr_rows_num dup(?)

  new_line db 13, 10  ; 13=CR:Carriage Return, 10=LF:Line Feed (Windows endings)
  db dollar
  space db ' '
  db dollar
  comma db ','
  db dollar

  msg db 'Enter the elements of the ', array_rows+48, 'x', array_cols+48, ' array:'
                                            ; 48 is added to the values to get
                                            ; their ASCII representation.
  db dollar
  msg_exit db '[0] | Exit.'
  db dollar
  msg_give db '[1] | Enter new elements.'
  db dollar
  msg_print_avg db '[2] | Print average.'
  db dollar
  msg_print_max db '[3] | Print max.'
  db dollar
  msg_print_min db '[4] | Print min.'
  db dollar
  msg_print_maxmin_sum db '[5] | Print sum of min & max.'
  db dollar
  msg_print_sum db '[6] | Print elements' sum.'
  db dollar
  msg_print_row db '[7] | Print specfic row.'
  db dollar
  msg_print_bsort db '[8] | Bubble-sort the array.'
  db dollar
  msg_print_freq db '[9] | Print frequency of specific element.'
  db dollar
  ; secret option...
  msg_print_pow2_avg db '[s] | Print square of average.'
  db dollar

  msg_sum db 'Elements' sum:'
  db dollar
  msg_avg db 'Elements' average:'
  db dollar
  msg_bsort db 'The array was bubble sorted!'
  db dollar
  msg_pow2_avg db 'Square of elements' average:'
  db dollar
  msg_max db 'Max number of elements:'
  db dollar
  msg_min db 'Min number of elements:'
  db dollar
  msg_maxmin_sum db 'Sum of max & min:'
  db dollar
  msg_row_in db 'Enter specific row (from 1 to ', array_rows+48, '):'
  db dollar
  msg_row_out db ' row:'
  db dollar
  msg_freq_in db 'Enter element:'
  db dollar
  msg_freq_out db 'Frequency of element:'
  db dollar
  msg_gotoMenu db 'Press any key, to return...'
  db dollar

  swap_count dw 1
  leading_zero db 1 ; this is a flag (true=1, false=0)

  ; specific return values:
  char_input db ?
  db dollar
  hex_input dw ?
  string_length dw ?
  cursor_row dw ?
  cursor_col dw ?
  cursor_pos dw ?
  ; ----

  sum dw 0
  db dollar
  average dw 0
  db dollar
  remainder dw 0
  db dollar
  power2_average dw 0
  db dollar
  max dw 0
  db dollar
  min dw 0
  db dollar
  maxmin_sum dw 0
  db dollar
  num_freq dw 0
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

  ; set ES register to memory location
  ; (equals to vidmem = 0b800h) of extra segment
  mov ax, vidmem
  mov es, ax

  call generate_lines ; proliferate with values the array 'lines'

  l_read_nums:
  call read_numbers

  l_disp_nums:
  call cls
  m_display_numbers 1, array_rows ; It displays the numbers,
                                  ; starting from the 1st row until the last.

  ; ---- PRECALCULATE ----
  call calc_sum                   ; modifies variables: 'sum'
  call calc_average_and_remainder ; modifies variables: 'average', 'remainder'
  call calc_quotient              ; modifies variables: 'quotient[]'
  call calc_pow2_avg              ; modifies variables: 'power2_average'
  call find_max_min_sum           ; modifies variables: 'max', 'min',
                                  ;                     'maxmin_sum'
  ; ----------------------

  ; Print author's name:
    m_print_at_line offset author_name, 0, text_color
    ; Prints author's name on the first line, and then 2 lines below,
    ; it prints the menu.
  ; MENU:
    ; Print 0th message; which tells how to exit.
    m_print_at_line offset msg_exit, 2, text_color
    ; Print 1st message; which tells how to give new numbers.
    m_print_at_line offset msg_give, 3, text_color
    ; Print 2nd message; which tells how to print the average.
    m_print_at_line offset msg_print_avg, 4, text_color
    ; Print 3rd message; which tells how to print the max.
    m_print_at_line offset msg_print_max, 5, text_color
    ; Print 4th message; which tells how to print the min.
    m_print_at_line offset msg_print_min, 6, text_color
    ; Print 5th message; which tells how to print the sum of max & min.
    m_print_at_line offset msg_print_maxmin_sum, 7, text_color
    ; Print 6th message; which tells how to print the sum of elements.
    m_print_at_line offset msg_print_sum, 8, text_color
    ; Print 7th message; which tells how to print a specific row of the array.
    m_print_at_line offset msg_print_row, 9, text_color
    ; Print 8th message; which tells how to bubble-sort the array.
    m_print_at_line offset msg_print_bsort, 10, text_color
    ; Print 9th message; which tells how to print a specific number's frequency.
    m_print_at_line offset msg_print_freq, 11, text_color
  ; ----

  ; Keyboard input detection, for menu item selection:
    l_kbd:
      mov ah, 6     ; direct console read/write
      mov dl, 0ffh  ; reads the console, sets AL to typed ASCII character
      int 21h
    je l_kbd        ; if ZF is set, i.e. no key was typed, then jump
      cmp al, '0' ; to exit the program
      je l_exit

      cmp al, '1' ; to read new numbers from the user
      je l_read_nums

      cmp al, '2' ; print average of elements
      je l_print_avg

      cmp al, '3' ; print max
      je l_print_max

      cmp al, '4' ; print min
      je l_print_min

      cmp al, '5' ; print sum of max & min
      je l_print_maxmin_sum

      cmp al, '6' ; print sum of elements
      je l_print_sum

      cmp al, '7' ; print specific row
      je l_print_row

      cmp al, '8' ; print bubble-sorted array
      je l_print_bubble_sort

      cmp al, '9' ; print frequency of specific number
      je l_print_freq

      cmp al, 's' ; secret selection to display average's power of 2...
      je l_print_pow2_avg
    jmp l_kbd
  ;----

  ; Options:
    ; #2 option
    l_print_avg:
      m_print_result offset msg_avg, average
      m_print_at_cursor offset comma, text_color
      m_printHex2Ascii quotient[0], 1
      m_printHex2Ascii quotient[2], 1
    jmp l_wait_for_key

    ; #3 option
    l_print_max:
      m_print_result offset msg_max, max
    jmp l_wait_for_key

    ; #4 option
    l_print_min:
      m_print_result offset msg_min, min
    jmp l_wait_for_key

    ; #5 option
    l_print_maxmin_sum:
      m_print_result offset msg_maxmin_sum, maxmin_sum
    jmp l_wait_for_key

    ; #6 option
    l_print_sum:
      m_print_result offset msg_sum, sum
    jmp l_wait_for_key

    ; #7 option
    l_print_row:
      ; 1st screen: user input
      call cls
      ; print message which prompts the user to enter a number
      m_print_at_line offset msg_row_in, 23, text_color

      call wait_for_input ; waits for input and stores it to char_input

      ; 2nd screen: display info & row
      call cls
      m_display_row_info offset char_input

      mov al, char_input
      and ax, 000fh ; Retain only the last digit of the hex, to convert it 
                    ; to a decimal.
                    ; (ASCII hex values of numbers: from 30h to 39h).

      m_display_numbers ax, 1 ; Display 1 specific row as given by
                              ; the user and saved on ax.

      ; print message how to go back to the menu
      m_print_at_line offset msg_gotoMenu, 0, text_color
    jmp l_wait_for_key

    ; #8 option
    l_print_bubble_sort:
      call cls                        ; first clear the screen
      call bubble_sort                ; bubble-sort the array
      m_display_numbers 1, array_rows ; display the numbers

      ; print message how to go back to the menu
      m_print_at_line offset msg_gotoMenu, 0, text_color

      ; print message to inform that the array has been bubble-sorted
      m_print_at_line offset msg_bsort, 24, text_color
    jmp l_wait_for_key

    ; #9 option
    l_print_freq:
      ; 1st screen: user input
      call cls
      ; print message which prompts the user to enter a number
      m_print_at_line offset msg_freq_in, 23, text_color

      call read_number  ; number is entered and stored to hex_input
      m_find_num_freq hex_input

      ; 2nd screen: display result
      call cls
      m_print_result offset msg_freq_out, num_freq
    jmp l_wait_for_key

    ; 's' option
    l_print_pow2_avg:
      m_print_result offset msg_pow2_avg, power2_average
    ;jmp l_wait_for_key

    l_wait_for_key:
      call wait_key_press
      m_set_cursor_pos 24, 0  ; cursor is set back to the bottom-left
                              ; position of the screen
    jmp l_disp_nums

    l_exit:
    call cls
    ret ; return to DOS
  ; -- end of options ---
main endp

; Subroutines
generate_lines proc near ; modifies variables: lines[]
  push ax
  push cx
  push di

  mov ax, 0             ; 0th line starts from 0
  mov cx, scr_rows_num  ; the screen has 25 lines/rows
  mov di, 0
  l_loop:
    mov lines[di], ax
    add ax, 160d  ; each line is 160 bytes (e.g. 0th line: 0-159, 1st: 160-319)
    add di, 2     ; Increase di by 2, because each element of 'lines' is
                  ; 2 bytes long (a word).
  loop l_loop

  pop di
  pop cx
  pop ax
  ret
generate_lines endp

read_number proc near ; modifies variables: hex_input
  push ax
  push bx
  push cx
  push dx

  mov cx, 10  ; Multiplication factor; later, by multiplying ax each time,
              ; the number (temporarily as the value of ax) is
              ; factored from units to thousands.
  mov bx, 0   ; Actual container of the number; via addition with
              ; ax it forms the number (in hex format).
  mov dx, 0   ; Number of digits typed by the user.

  m_set_cursor_pos 24, 0  ; ensure cursor is at the bottom-left of the screen

  l_read_num:    
    call get_default_cursor_pos ; cursor_pos = default cursor's position
    m_paint_area text_color, cursor_pos, 1  ; paint that single cell where the
                                            ; cursor is located

    mov ah, 1 ; Read the keyboard, stores string input (a single char) to al,
              ; echoes/displays character on screen.
    int 21h   ; (It doesn't wait to press ENTER, thus when you press a key,
              ; it immediately takes that as input and continues).
    mov ah, 0 ; AH is set to 0 (from 1), because later ax is used to
              ; construct (via addition with bx) the typed number.
    
    inc dx    ; A digit was typed, increase dx by 1.

    ; Checks to see if typed digit is out of range of accepted digits (0-9),
    ; if so then jump.
    cmp al, '0'
    jb l_store_read_num
    cmp al, '9'
    ja l_store_read_num
    ; If ENTER is pressed, which is equal to 0Dh, it will jump, too.
    
    cmp dx, 5             ; check if the user has typed a 5th digit
    jge l_store_read_num  ; if so, store only the 4 typed digits (which is the
                          ; limit, i.e. a number is consisted only of 4 digits)

    sub al, '0' ; convert ASCII char to hex number
                ; (e.g.: ('1'=31h) - ('0'=30h) = 1h)

    ; Multiply BX by 10 ( = CX):
    push dx     ; the instruction 'mul cx' below also modifies dx
    push ax
    mov ax, bx  ; ax copies bx
    mul cx      ; multiply ax with cx, and store result to ax
    mov bx, ax  ; BX (which is the actual number container) copies ax
                ; (here, ax is used as a temp register for the multiplication,
    pop ax      ; while its actual value is retained on the stack).
    pop dx

    add bx, ax  ; The previously typed digits, contained by bx, and constructed
                ; so they form a number, where each typed digit takes the right
                ; place in the number via the above multiplication (mul cx),
                ; is added with ax, which currently contains the last typed
                ; digit (as the current units).
  jmp l_read_num

  l_store_read_num:
  mov hex_input, bx

  m_set_cursor_pos 24, 0  ; ensure cursor is at the bottom-left of the screen
                          ; (again, because it has been moved)

  pop dx
  pop cx
  pop bx
  pop ax
  ret
read_number endp

read_numbers proc near ; modifies variables: array[]
  push ax
  push di

  mov di, 0 ; counter of elements/numbers (with step 2); mainly, the destination
            ; index of the array where numbers are stored
  l_read_nums_loop:
    call cls
    ; 'Enter elements of NxN array:'
    m_print_at_line offset msg, 23, text_color

    call read_number
    mov ax, hex_input
    mov array[di], ax
    add di, 2           ; AX is 16 bit (2 bytes), so it takes 2 locations/bytes
                        ; to be saved, and that's why di is increased by 2.
    cmp di, array_size  ; And that's why it checks for elements_num*2=array_size
                        ; (here it has nothing to do with color).
  jne l_read_nums_loop

  pop di
  pop ax
  ret
read_numbers endp

display_numbers proc near ; has macro, parameters: row_index, rows_num
  ; row_index = [bp+6]
  ; rows_num  = [bp+4]
  push bp     ; [bp+0]
  mov bp, sp

  push ax
  push bx
  push si

  m_set_cursor_pos 24, 0    ; ensure cursor is at the bottom-left of the screen

  mov ax, [bp+6]            ; ax = row_index
  dec ax                    ; Decrease ax by 1, because user enters numbers
                            ; from 1 to array_cols, but rows' indexes
                            ; start from 0 to (array_cols-1).
  imul ax, array_cols*2     ; ax = row_index * array_cols * 2
                            ; (multiplied also by 2, because each number
                            ; takes 2 bytes in memory, so the index of
                            ; each number takes a step of 2)
  mov si, ax                ; Source index of which element/number
                            ; to retrieve first from array, based on the
                            ; row index given and the number of array's columns. 

  mov ax, 0                 ; number of rows currently displayed
  mov bx, 0                 ; Counter of how many numbers have been
                            ; displayed in the current row.
  l_disp_nums_loop:
    m_printHex2Ascii array[si], 4 ; prints a number (of 4 digits)
                                  ; in ascii format
    m_print_space

    add si, 2           ; increase si by 2 to get the next number
                        ; in the next iteration

    inc bx              ; A number has been displayed, so bx is increased by 1.
    cmp bx, array_cols  ; <-Is the limit of how many numbers (of 4 digits)
                        ; should be displayed in the current row,
                        ; once bx reach this, the code continues below,
  jne l_disp_nums_loop  ; else, reiterate.
    m_print_new_line
    mov bx, 0           ; reset bx, for the next iteration
    
    inc ax              ; increase the number of displayed rows by 1
    cmp ax, [bp+4]      ; Check if the limit of how many rows
                        ; should be displayed has been reached,
    je l_break          ; if so, break the loop
    cmp si, array_size  ; else, check if the index (si) is equal to the number
                        ; of bytes (the size) of the array
  jne l_disp_nums_loop  ; if not, reiterate
                        ; else...
  l_break:
  pop si
  pop bx
  pop ax
  pop bp
  ret 4 ; 2 parameters * 2 bytes = increase SP by 4 after return
display_numbers endp

display_row_info proc near ; has macro, parameters: row_char_offset
  ; row_char_offset : [bp+4]
  push bp           ; [bp+0]
  mov bp, sp

  push ax

  mov ax, lines[23*2]
  ; In the above instruction, lines[23*2] refers to an address and not
  ; to a value, therefore if you write lines[23*2]+2 it will add 2 to the
  ; address and not to the value, which is equivalent to lines[24*2],
  ; (which is equivalent to lines[23*2+2] or lines+23*2+2).

  ; print message about which row is being displayed
  m_print [bp+4], ax, text_color
  
  ; We want to write the next text/string just one position to the right of
  ; the above text (that is 2 bytes higher in the memory).
  add ax, 2 ; And that's why we need to add 2 to the value (saved
            ; on ax) with a seperate instruction.
  m_print offset msg_row_out, ax, text_color

  pop ax
  pop bp
  ret 2 ; 1 parameter * 2 bytes = increase SP by 2 after return
display_row_info endp

calc_sum proc near  ; modifies variables: sum
    push ax
    push cx
    push si

    ; Calculation of the sum:
    mov ax, 0 ; register to contain the sum
              ; (it will be overflowed if the numbers are big)
    mov si, 0 ; index to array's element, starting from the first (position 0)
    mov cx, elements_num ; number of iterations, equal to element's number
    l_sum_loop:
      add ax, array[si]
      add si, 2 ; each number is 2 bytes long, so si is increased by 2
    loop l_sum_loop

    mov sum, ax	; copy the sum from the accumulator (ax)
                ; to its dedicated variable

    pop si
    pop cx
    pop ax
    ret
calc_sum endp

calc_average_and_remainder proc near  ; modifies variables: average, remainder
  push ax
  push cx
  push dx

  mov dx, 0
  mov ax, sum
  mov cx, elements_num
  div cx  ; dx:ax = dx:ax/cx        =>
          ; sum = sum/elements_num  =>
          ;                         => ax = average (quotient), dx = remainder
  mov average, ax
  mov remainder, dx

  pop dx
  pop cx
  pop ax
  ret
calc_average_and_remainder endp

calc_quotient proc near  ; modifies variables: quotient[]
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
    je l_no_remainder

    add di, 2
    cmp di, 4
  jne l_cq

  l_no_remainder:
  pop di
  pop dx
  pop cx
  pop ax
  ret
calc_quotient endp

calc_pow2_avg proc near  ; modifies variables: power2_average
  push ax
  
  mov ax, average
  mul ax
  mov power2_average, ax
  
  pop ax
  ret
calc_pow2_avg endp

find_max_min_sum proc near  ; modifies variables: max, min, maxmin_sum
  push ax
  push bx
  push si

  mov ax, array[0]  ; the first element is considered the currently max value
  mov bx, array[0]  ; the first element is considered the currently min value
  mov si, 2         ; we start from the second element
  l_next_elem:
    ; find max:
    cmp ax, array[si] ; compare current max (ax) w/ current element (array[si]) 
    jge l_find_min    ; if current max >= current element, then
                      ; jump to next comparison (find min),
    mov ax, array[si] ; else, the new max is the current element
    l_find_min:
    cmp bx, array[si] ; compare current min (bx) w/ current element (array[si]) 
    jle l_inc_index   ; if current min <= current element, then
                      ; jump to l_increment the index (si),
    mov bx, array[si] ; else, the new min is the current element
    ; increment index:
    l_inc_index:
    add si, 2         ; each element is 2 bytes long (a word)
    cmp si, array_size
  jne l_next_elem

  mov max, ax         ; we store the max value we found
  mov min, bx         ; we store the min value we found
  add ax, bx          ; calculate the sum of max and min
  mov maxmin_sum, ax  ; store that sum

  pop si
  pop bx
  pop ax
  ret
find_max_min_sum endp

; has macro, parameters: number
find_num_freq proc near ; modifies variables: num_freq
  ; number  : [bp+4]
  push bp   ; [bp+0]
  mov bp, sp

  push bx
  push cx
  push si

  mov cx, 0       ; frequency counter
  mov bx, [bp+4]  ; bx = number
  mov si, 0       ; source index
  l_next_elem:
    cmp bx, array[si]   ; compare given number (bx) with number of array
    jne l_inc_index     ; if they're not equal, then jump to l_inc_index
    inc cx              ; else, increase the frequency counter (cx) by 1
    l_inc_index:
    add si, 2           ; each element is 2 bytes long (a word)
    cmp si, array_size
  jne l_next_elem       ; If source index isn't equal to array's byte size,
                        ; then jump to compare the next element of the array.

  mov num_freq, cx  ; store the frequency

  pop si
  pop cx
  pop bx

  pop bp
  ret 2 ; 1 parameter * 2 bytes = increase SP by 2 after return
find_num_freq endp

bubble_sort proc near ; modifies variables: array[]
  push ax
  push bx
  push di

  mov swap_count, 1
  ; Re-initialize swap_count to 1 every time you run a
  ; bubble sort, otherwise it won't work if you try to run
  ; this procedure for more than one time, because swap_count
  ; will be 0 at the end of the first execution of this procedure,
  ; and thus when it'll check below if it's 0, it will stop the
  ; algorithm and no sorting will ever happen again.

  ; ---- actual sorting starts here ----
  l_next_pass:
  cmp swap_count, 0   ; Checks to see how many swaps have been made
                      ; in the current pass.
  je l_done           ; If no swaps, then the array is sorted,
                      ; and thus jump to 'l_done'.
  mov swap_count, 0   ; If the array is not sorted yet,
                      ; then reset swap_count to 0.
  mov di, 0           ; Index to array's element,
                      ; starting from the first (position 0),
                      ; also counter of how many elements have been checked.

  l_next_elem:
  mov ax, array[di]
  cmp ax, array[di+2] ; compare the first number with the next one
                      ; (each element/number takes 2 bytes in memory)
  jg l_swap           ; If the first is greater than the next one,
                      ; then jump to the label 'l_swap', where the swapping of
                      ; the two numbers takes place. The two numbers exchange
                      ; places in the array.

  l_increment:
  add di, 2
  cmp di, last_element_index  ; checks if di is equal to the index of the
                              ; last element              
  je l_next_pass              ; if so, jump to 'l_next_pass'
  jmp l_next_elem             ; else, jump to 'l_next_elem'
  
  ; Given that the addresses begin from 0, that each number takes 2 places/bytes
  ; in memory, it is clear that the last element (in this case), instead of
  ; taking the places <59 & 60> (i.e. to begin from <59>), [since the addresses
  ; begin from 0], it takes the places <58 & 59> (i.e. it begins from place
  ; <58>, that is a place backwards (minus 1 place, in relation to <59>).
  ; Moreover, since the check of the elements is done in pairs, then the last
  ; check that will happen, it will be between these places:
  ; <56 & 57> and <58 & 59>.
  ;   That is, as for index DI, the last value that it (must) take is <56>,
  ;   so that, when it will need to read the next element (which will also be
  ;   the last) using the formula <DI+2>, it will be the one that begins from
  ;   place <58>.
  ; If the check used to stop the loop is "DI is equal to array_size?", then
  ; it won't run correctly, because it will check if DI is equal to 60. The
  ; variable array_size is 60 because it is concerned with the size of the array
  ; in bytes, and not with which is the maximum index number of the array.

  l_swap:
    mov bx, array[di+2] ; bx plays the role of the temp variable here
    mov array[di+2], ax ; ax already has the value of 'array[di]'
                        ; from 'l_next_elem'
    mov array[di], bx
    inc swap_count  
  jmp l_increment
  ; ---- end of sorting ----

  l_done:
  pop di
  pop bx
  pop ax
  ret
bubble_sort endp

find_string_length proc near  ; has macro, parameters: string_offset
  ; string_offset : [bp+4]
  push bp         ; [bp+0]
  mov bp, sp

  push cx
  push si

  mov si, [bp+4]  ; string offset
  mov cx, 0       ; counter of bytes, equal to the length of the string

  l_loop:
    cmp byte ptr[si], dollar
    je l_break
    inc si
    inc cx
  jmp l_loop

  l_break:
  mov string_length, cx
  
  pop si
  pop cx
  pop bp
  ret 2 ; 1 parameter * 2 bytes = increase SP by 2 after return
find_string_length endp

print_new_line proc near
  push ax
  push dx

  mov ah, 09h             ; display a text string
                          ; (must end with an ASCII $ (24H))
  mov dx, offset new_line ; offset of text from segment (where data is located),
                          ; dx's value is the address of the text string
  int 21h
  ; it prints a new line where the default cursor currently is

  m_set_cursor_pos 24, 0  ; set cursor to bottom-left position on screen

  pop dx
  pop ax
  ret
print_new_line endp

print proc near ; has macro, parameters: string_offset, position, color
  ; string_offset : [bp+8]
  ; position      : [bp+6]
  ; color         : [bp+4]
  push bp         ; [bp+0]
  mov bp, sp

  push ax
  push di
  push si

  mov si, [bp+8]  ; string offset
  mov di, [bp+6]  ; position on screen
  mov ah, [bp+4]  ; text color (the high byte of ax will
                  ; always contain the color value)
  
  l_loop:
    mov al, byte ptr[si]  ; al copies a single character/byte from the string
    cmp al, dollar        ; Compare that character (al) with '$',
    je l_break            ; if they're the same, then break.

    ; ; check if char is a space,
    ; ; if so change text color temporarily to black(0) backg. & white(7) foreg.
    ; push ax ; in case color will change, push old value onto the stack
    ; cmp al, space     ; check that
    ; jne l_dntChngClr  ; if not, don't change the color
    ; mov ah, 07h       ; else, change it

    ; l_dntChngClr:
    mov es:[di], ax ; es:di <- ax (ah: color, al: character)
    add si, 1
    add di, 2

    ; pop ax  ; in case color was changed, pop the original value back to ax,
    ;         ; else, it just remains the same
  jmp l_loop

  l_break:
  pop si
  pop di
  pop ax

  pop bp
  ret 6 ; 3 parameters * 2 bytes = increase SP by 6 after return
print endp

print_result proc near  ; has macro, parameters: msg_offset, result
  ; msg_offset  : [bp+6]
  ; result      : [bp+4]
  push bp       ; [bp+0]
  mov bp, sp

  call cls  ; first clear the screen
  ; Display message which tells that in the next line the result is displayed:
  m_print_at_line [bp+6], 23, text_color  ; [bp+6] = msg_offset
  ; print the result in that next line:
  m_printHex2Ascii [bp+4], 5              ; [bp+4] = result
                                          ; displayed with 5 digits

  ; print message in the first line, which tells how to go back to the menu:
  m_print_at_line offset msg_gotoMenu, 0, text_color

  pop bp
  ret 4 ; 2 parameters * 2 bytes = increase SP by 4 after return
print_result endp

printHex2Ascii proc near  ; has macro, parameters: hex, digits_num
  ; hex         : [bp+6]
  ; digits_num  : [bp+4]
  push bp       ; [bp+0]
  mov bp, sp

  push ax
  push bx
  push cx
  push dx

  ; The 1st parameter 'hex' is located 6 bytes above where the stack pointer
  ; currently points, and the 2nd parameter 'digits_num' 4 bytes.
  ; That's because previously onto the stack has been pushed (in descending
  ; order):
  ; [bp+2] => return address (where the call to this subroutine was made)
  ; [bp+0] => old base pointer
  mov ax, [bp+6]  ; hex
  mov dx, 0

  mov bx, 1       ; initial value of bx
  mov cx, [bp+4]  ; digits_num
  dec cx          ; CX is decreased by 1, to produce the right divisor
                  ; (using the loop 'l_mulbx'), that is to prevent the 
                  ; multipl. if cx/digits_num equals 1 (as given by the user).
  cmp cx, 0       ; If cx = 0, then,
  je l_setcxbybx  ; don't multiply bx (but just set cx equals to bx),
                  ; else, multiply bx by 10 each time (times equal to cx):
  l_mulbx:
    imul bx, 10
  loop l_mulbx

  l_setcxbybx:
  mov cx, bx  ; CX now contains the divisor for the first
              ; division with dx-ax (the divident) below.
  
  mov leading_zero, 1 ; If the first digit is a zero,
                      ; it's considered as a leading zero.
  l_print_loop:
    mov dx, 0 ; We don't care about the quotient (which after the xchg is
              ; now on dx), and also it prevents problems with 
              ; the division (div cx), because otherwise if dx is not 0,
              ; the dividend (dx-ax) would become a quite large and
              ; unwanted/wrong number, and the program will crash.
              ; Especially when the divisor (cx) equals 1, the quotient (ax)
              ; would be equal to the dividend (dx-ax) and as it's obvious
              ; ax (16 bit) cannot store dx-ax (32 bit).
    div cx    ; dx-ax / cx => dx = remainder, ax = quotient

    push ax
    push ax ; AX is pushed twice, because the called subroutine below, 
            ; when it returns, discards (with 'ret 2') what was just pushed onto
            ; the stack before it was called (actually, 'ret 2' increases
            ; the stack pointer by 2).
    call hex2ascii  ; 'call' first pushes the current address onto the stack,
                    ; then does an unconditional jump to the specified label
                    ; (i.e. the name of the subroutine).

    ; ---- convert leading zeros (trim e.g. 0009 to    9, 0125 to  125 etc) ----
    mov al, buffer[3] ; Because we have defined buffer to contain elements
                      ; of 1 byte, we can't copy an element from buffer
                      ; to ax (which the latter is 2 bytes).
                      ; Therefore, we have to use al (which is 1 byte) and
                      ; complies with the size of buffer's elements.
    cmp al, '0'               ; Check if digit is 0, and
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
    m_print_at_cursor offset buffer[3], text_color

    ; pseudocode:
    ; if (digit != 0) then
    ;   set leading_zero equal to 0/false
    ;   display the digit
    ; else if (digit == 0 AND leading_zero == 0/false) then
    ;   display the digit
    ; else if (digit == 0 AND it is the last digit) then
    ;   display the digit
    ; else  ; (digit == 0 AND leading_zero == 1/true)
    ;   convert digit from 0 to space
    ;   display space
    ; end
    ; ---- conversion ends here ----
    pop ax

    xchg dx, ax ; ax now has the remainder (and dx the quotient)

    ; check condition:
    cmp cx, 1 ; When cx is 1, there is no meaning anymore to perform the
              ; divisions, thus the procedure is considered finished.
    je l_end_proc

    ; divide cx by 10:
    push dx
    push ax
    mov dx, 0
    mov ax, cx
    mov cx, 10
    div cx
    mov cx, ax
    pop ax
    pop dx
    ; ----
  jmp l_print_loop
  
  l_end_proc:
  ; restore... (popping in the reverse order compared to pushing)
  ; ...the values of the registers:
  pop dx
  pop cx
  pop bx
  pop ax
  ; ...the old base pointer:
  pop bp
  ret 4	; What happpens with this, (probably) in the following order:
        ; * Pop from the stack the return address and save it to
        ; the IP (instruction pointer), and increase SP (stack pointer) by 2.
        ; * Increase SP by 4, effectively removing the 2 parameters that were
        ; pushed onto the stack before the call of this subroutine.
printHex2Ascii endp

cls proc near
  push ax
  push cx
  push di

  mov al, space
  mov ah, cls_color

  mov di, 0
  mov cx, scr_size
  rep stosw ; es:di <- ax, di <- di+2

  pop di
  pop cx
  pop ax
  ret
cls endp

wait_for_input proc near ; modifies variables: char_input 
  push ax

  mov ah, 07h ; direct console input without echo
  int 21h

  mov char_input, al

  pop ax
  ret
wait_for_input endp

wait_key_press proc near
  push ax

  ; wait for any key press
  mov ah, 07h
  int 21h

  pop ax
  ret
wait_key_press endp

paint_area proc near  ; has macro, parameters: color, position, len
  ; color     : [bp+8]
  ; position  : [bp+6]
  ; len       : [bp+4]
  push bp     ; [bp+0]
  mov bp, sp

  push ax
  push cx
  push di

  mov al, space   ; character = space
  mov ah, [bp+8]  ; color
  mov di, [bp+6]  ; position
  mov cx, [bp+4]  ; len (length)
  rep stosw ; es:di <- ax, di <- di+2

  pop di
  pop cx
  pop ax

  pop bp
  ret 6 ; 3 parameters * 2 bytes = increase SP by 6 after return
paint_area endp

set_cursor_pos proc near
  ; row     : [bp+6]
  ; column  : [bp+4]
  push bp   ; [bp+0]
  mov bp, sp

  push ax
  push bx

  mov ax, [bp+6]      ; You can't move a value from memory to memory,
  mov cursor_row, ax  ; and that's why registers are used in between.
  mov bx, [bp+4]
  mov cursor_col, bx

  call calc_cursor_pos
  call set_default_cursor_pos ; for interoperability

  pop bx
  pop ax

  pop bp
  ret 4 ; 2 paremeters * 2 bytes = increase SP by 4 after return
set_cursor_pos endp

calc_cursor_pos proc near
  push ax
  push bx

  mov ax, cursor_row
  mov bx, cursor_col

  imul ax, 160  ; 160 = scr_cols_num * 2 = 80 * 2
  imul bx, 2
  add ax, bx
  mov cursor_pos, ax ; cursor_pos = (cursor_row*160) + (cursor_col*2)

  pop bx
  pop ax
  ret
calc_cursor_pos endp

get_default_cursor_pos proc near
  push ax
  push bx
  push cx
  push dx

  ; get cursor position:
  mov ah, 3h  ; subfunction (video - get cursor position and size)
  mov bh, 0   ; display page number
  int 10h

  mov ax, dx
  shr ax, 8
  mov cursor_row, ax  ; dh : row
  mov ax, dx
  and ax, 00ffh
  mov cursor_col, ax  ; dl : column

  call calc_cursor_pos

  pop dx
  pop cx
  pop bx
  pop ax
  ret
get_default_cursor_pos endp

set_default_cursor_pos proc near
  push ax
  push bx
  push cx
  push dx
  
  mov bx, cursor_row
  ; bl now contains the row
  mov cx, cursor_col
  ; cl now contains the column

  ; set cursor position:
  mov ah, 2h      ; subfunction code
  mov dh, bl      ; row
  mov dl, cl      ; column
  mov bh, 0       ; display page number
  int 10h         ; https://en.wikipedia.org/wiki/INT_10H

  pop dx
  pop cx
  pop bx
  pop ax
  ret
set_default_cursor_pos endp

HEX2ASCII PROC NEAR ; modifies variables: buffer[]
  ; Converts a word variable to ASCII
  ; Writes the results in the BUFFER (4 bytes) variable
  ; PARAMETERS
  ; gets a word variable from the stack
  push bp
  mov bp, sp
  mov ax, [bp+4]  ; take input variable
                  ; It takes the ax value that was pushed last
                  ; before this subroutine was called.
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
  ret 2 ; ... may optionally specify an immediate operand. By adding a constant,
        ; it effectively removes any arguments that the calling program pushed
        ; on the stack before the execution of the call instruction.
        ; Effectively, in this case, it removes the data that was pushed onto
        ; the stack (e.g. 'push ax'), before this subroutine was called.
HEX2ASCII ENDP

code ends
end start

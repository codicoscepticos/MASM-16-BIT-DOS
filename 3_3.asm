.486
assume cs:code, ds:data, ss:stack

; Constants
vidmem equ 0b800h
scrw equ 80*25

color equ 7
color1 equ 12h
color2 equ 74h

eom equ 0

; Segments
stack segment para stack
    db 256 dup (0)
stack ends

data segment para public
    line1 dw 160d
    line2 dw 320d
    msg1 db 'My name is'
    db eom
    msg2 db 'Name Surname'
    db eom
data ends

code segment USE16 para public 'code'
start:
    main proc far
        mov ax, data
        mov ds, ax

        mov ax, vidmem
        mov es, ax

        call cls
        call printstr1
        call printstr2

        ; exit
        mov ah, 4Ch
        int 21h
    main endp

    ; Subroutines
    cls proc near
        mov di, 0

        mov al, ' '
        mov ah, color

        mov cx, scrw
        rep stosw ; es:di <- ax, di <- di+2

        ret
    cls endp

    printstr1 proc near
        lea si, msg1 ; load effective address, lea reg,mem, si: source index
        
        mov di, line1
        mov ah, color1
        mov cx, 10d
        rep stosw
        mov di, line1

        print_loop:
            cmp byte ptr[si], eom ; cmp mem, imm(ediate)
            je prints_exit
            movsb ; move string byte/data, es:di <- ds:si, di <- di+1, si <- si+1
            inc di
        jmp print_loop
        
        prints_exit:
        ret
    printstr1 endp

    printstr2 proc near
        lea si, msg2
        
        mov di, line2
        mov ah, color2
        mov cx, 21d
        rep stosw
        mov di, line2

        prints_loop:
            cmp byte ptr[si], eom
            je prints_exit
            movsb
            inc di
        jmp prints_loop
        
        prints_exit:
        ret
    printstr2 endp

code ends
end start
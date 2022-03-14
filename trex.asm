[org 0x0100]
jmp start
message: db 'PRESS ANY KEY TO PLAY'
message1: db 'TREX GAME'
message2: db 'GAME OVER'
message3: db 'SCORE:'
tickcount: dw 0
oldisr: dd 0 ; space for saving old isr

printnum: push bp
mov bp, sp
push es
push ax
;push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax

mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov di, 140 ; point di to 70th column
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x07 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack

;COMPARING IF SPACE IS PRESSED OR NOT OR IF HURDLE AND DINASOUR COLLIDED OR NOT
cmp word [es:1768], 0x0701   ;if space is pressed dinasour face will be above i.e. at jump location
jne printhurdles
cmp word [es:1928], 0x07B3   ;WHEN HURDLE COLLIDE WITH DINASOUR ITS BODY OTHER THEN FACE ERASES AND IF SO THEN END THE GAME
jne tyt1
cmp word [es:2088], 0x0713
jne tyt1

printhurdles:
pop di
mov bx,2236

l1:
;Printing hurdle 1
mov word[es:bx+si],0x0720
sub si,2
mov word[es:bx+si],0x07DB
add si,2
mov bx,2076
mov word[es:bx+si],0x0720
sub si,2
mov word[es:bx+si],0x07DB
;Printing hurdle 2
mov bx,2160
mov word[es:bx+di],0x0720
sub di,2
mov word[es:bx+di],0x07DB
;end conditions for hurdles
hh:
mov cx,0 
mov bx,2160

mov ax,0
add ax,bx
add ax,di
cmp ax,2080
jne dd1
mov word [es:bx+di],0x0720
mov di,76

dd1:
mov bx,2236
mov ax,0
add ax,bx
add ax,si
cmp ax,2080
jne endd
mov word[es:bx+si],0x0720
mov bx,2076
mov word[es:bx+si],0x0720
mov si,0
jmp endd

tyt1:
jmp endthegame
endd:
pop dx
pop cx
;pop bx
pop ax
pop es
pop bp
ret 2

clrscr: 
	 push es
	 push ax
	 push cx
	 push di
	 mov ax, 0xb800
	 mov es, ax ; point es to video base
	 xor di, di ; point di to top left column
	 mov ax, 0x0720 ; space char in normal attribute
	 mov cx, 2000 ; number of screen locations
	 cld ; auto increment mode
	 rep stosw ; clear the whole screen
	 pop di 
	 pop cx
	 pop ax
	 pop es
	 ret

printdashes:
push es
push di
push ax
push cx
mov ax,0xb800
mov es,ax
mov al,0x2d
mov ah,07
xor di,di
mov di,2240
mov cx,80
forloop
mov [es:di],ax
add di,2
loop forloop
pop cx
pop ax
pop di
pop es
ret

printscore: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit1: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit1 ; if no divide it again
mov di, 2480 ; point di to 70th column
nextpos1: pop dx ; remove a digit from the stack
mov dh, 0x0F ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos1 ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2

endthegame:
call clrscr
endgame:
;using BIOS VIDEO SERVICE TO PRINT STRING
mov ah,0x13
mov al,0
mov bh,0
mov bl,0x07
mov dx,0X0D20
mov cx,9
push cs
pop es
mov bp,message2
int 0x10
;using BIOS VIDEO SERVICE TO PRINT STRING
mov ah,0x13
mov al,0
mov bh,0
mov bl,0x0F
mov dx,0X0F20
mov cx,6
push cs
pop es
mov bp,message3
int 0x10
push word [cs:tickcount]
call printscore
jmp endgame  ;infinite loop
	 
kbisr: push ax
push es
mov ax, 0xb800
mov es, ax ; point es to video memory
mov word [es:1768], 0x0701
mov word [es:1928],  0x07B3
mov word [es:2088], 0x0713
in al, 0x60 ; read a char from keyboard port
cmp al, 0x39 ; has the SPACE KEY PRESSED
jne nextcmp ; no, try next comparison
 mov word [es:1768], 0x0720
 mov word [es:1928], 0x0720  ;if space is pressed erase the previous dinasour and print at jump location
 mov word [es:2088], 0x0720
 
mov word [es:808], 0x0701
mov word [es:968],  0x07B3
mov word [es:1128], 0x0713

jmp exit 

nextcmp: cmp al, 0xB9 ; has the space released
jne nomatch ; no, chain to old ISR
mov word [es:808], 0x0720
mov word [es:968],  0x0720   ;if space is released erase the jump dinasour and print at previous location
mov word [es:1128], 0x0720

mov word [es:1768], 0x0701
mov word [es:1928],  0x07B3
mov word [es:2088], 0x0713
jmp exit ; leave the interrupt routine
nomatch: pop es
pop ax
jmp far [cs:oldisr] ; call the original ISR

exit: mov al, 0x20
out 0x20, al ; send EOI to PIC
pop es
pop ax
iret ; return from interrupt

timer:
push ax
inc word [cs:tickcount]; increment tick count
push word [cs:tickcount]
call printnum ; print tick count
mov al, 0x20
out 0x20, al ; end of interrupt
pop ax
iret ; return from interrupt

start:
call clrscr
;using BIOS VIDEO SERVICE TO PRINT STRING
mov ah,0x13
mov al,0
mov bh,0
mov bl,0x0F
mov dx,0X0020
mov cx,9
push cs
pop es
mov bp,message1
int 0x10
;using BIOS VIDEO SERVICE TO PRINT STRING
mov ah,0x13
mov al,0
mov bh,0
mov bl,0xFF
mov dx,0X0A19
mov cx,21
push cs
pop es
mov bp,message
int 0x10
;GETTING KEYSTROKE BY USING BIOS KEYBOARD SERVICE
mov ah,0
int 0x16
call clrscr
call printdashes
mov di,0
xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [es:9*4]
mov [oldisr], ax ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldisr+2], ax ; save segment of old routine
cli ; disable interrupts
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2
mov word [es:8*4], timer; store offset at n*4
mov [es:8*4+2], cs ; store segment at n*4+2
sti ; enable interrupts

mov ax, 0x4c00 ; terminate and stay resident
int 0x21


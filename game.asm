[org 0x0100]
jmp start

tick : dw 0
di_count : dw 0
oldisr : dw 0
Dir : dw 0
TFlag : dw 0
score : dw 0
scoreDisplay : db 'Score is : '
length : dw 11


clrscr:       push es 
              push ax 
              push cx 
              push di 
 
              mov  ax, 0xb800 
              mov  es, ax             ; point es to video base 
              xor  di, di             ; point di to top left column 
              mov  ax, 0x0720         ; space char in normal attribute 
              mov  cx, 2000           ; number of screen locations 
 
              cld                     ; auto increment mode 
              rep  stosw              ; clear the whole screen 
 
              pop  di 
			  pop  cx 
              pop  ax 
              pop  es 
              ret 

;-----------------------------------------------------------------------------------------------------------------------------

SetBackground :    push ax
                   push es
				   push di
				   push si
				   push cx
				   
				   mov ax, 0xb800
				   mov es, ax
				   
				   xor ax, ax
				   mov di, ax
				   
firstHalf :        
                   mov word [es:di] , 0x2700  ;green cell
                   add di, 2
				   mov word [es:di] , 0x2700
				   add di, 2
				   cmp di, 4000
				   jae exitFunction
				   
secondHalf :       
                   mov word [es:di] , 0x2700
                   add di, 2
				   mov word [es:di] , 0x2700
				   add di, 2
				   mov word [es:di] , 0x4700   ; red cell
				   add di, 2
				   mov word [es:di] , 0x2700
				   add di, 2
				   cmp di, 4000
				   jae exitFunction
				   jmp firstHalf
				   
				   
exitFunction :	   mov di, 132
                   mov si, scoreDisplay
				   mov cx, [length]
				   mov ah, 0x07
				   
				   cld 
				   
nextchar :         lodsb
                   stosw
				   loop nextchar
				   
				   call DisplayScore
                   
				   pop cx
                   pop si
				   pop di
				   pop es
				   pop ax
				   ret
              

;------------------------------------------------------------------------------------------------------------------------------------

DisplayScore :    push ax
                  push bx
				  push cs
				  push dx
                  push es
				  push di
				  
				  mov di, 154 
                  mov ax, 0xb800
                  mov es, ax
                  mov word ax, [score]
				  
				  mov  bx, 10             ; use base 10 for division 
                  mov  cx, 0              ; initialize count of digits 
 
nextdigit:        mov  dx, 0              ; zero upper half of dividend 
                  div  bx                 ; divide by 10 
                  add  dl, 0x30           ; convert digit into ascii value 
                  push dx                 ; save ascii value on stack 
                  inc  cx                 ; increment count of values  
                  cmp  ax, 0              ; is the quotient zero 
                  jnz  nextdigit          ; if no divide it again 
 

nextpos:          pop  dx                 ; remove a digit from the stack 
                  mov  dh, 0x07           ; use normal attribute 
                  mov [es:di], dx         ; print char on screen 
                  add  di, 2              ; move to next screen location 
                  loop nextpos 				  

                 

                  pop di
                  pop es
				  pop dx
				  pop cx
				  pop bx
                  pop ax
                  ret				  

;------------------------------------------------------------------------------------------------------------------------------------

Movement :      push ax
                push es
				push di
				
				mov di, [di_count]  ;di count is stored before for positioning
				cmp di, 158  ;sees if last column of first row
				je origin 
				jmp ContinueMove
				
origin : 		mov di, 0  ;brings back to first column (0,0)
				
ContinueMove:	mov ax, 0xb800
				mov es, ax
				
                cmp word [es:di], 0x2700 ;check for green cell
				jne noMatch
				inc word [score]
				jmp MovInScreen
				
noMatch :       cmp word [es:di], 0x4700 ;check for red
                jne MovInScreen  ;black screen 
				mov word [es:di] , 0x072A
                pop di
				pop es
				pop ax
                jmp far [cs:oldisr]	 ;terminate the program and load old isr

MovInScreen :	mov word [es:di], 0x072A
				add di, 2
				
				cmp di, 2
				je skipForFirst
				
				
				sub di, 4
				mov word [es:di], 0x0020
				add di, 4

skipForFirst:	mov word [di_count], di
                call DisplayScore
				
				pop di
				pop es
				pop ax
				ret
				
;----------------------------------------------------------------------------------------------------------------------------------------

asteriskMove :  push ax
                push es
				push di
				
				mov di, [di_count]
				
				;boundary checks for up and down
				cmp di, 4000 ; out of last row
				jae OrganizeUp
				cmp di, 0    ;out of first row
				jb OrganizeDown
				jmp continue
				
OrganizeUp :    sub di, 4000 ;brings to the start of the column
                jmp continue


OrganizeDown :  add di, 4160

				
continue :		mov ax, 0xb800
				mov es, ax
				
				mov word [es:di], 0x0020   ;moves a space in the position before
				
				cmp word [Dir], 1
				je Up
				cmp word [Dir], 2
				je Down
				cmp word [Dir], 3
				je Left
				cmp word [Dir], 4
				je Right
				
Up :            sub di, 160
                jmp scoreCheck

Down :          add di, 160
                jmp scoreCheck

Left :          sub di, 2
                jmp scoreCheck

Right :         add di, 2	


scoreCheck :    cmp word [es:di], 0x2700   ;check for green
				jne nextCMP
				inc word [score]
                jmp print_asterisk

nextCMP :       cmp word [es:di] , 0x4700  ;check for red cell
                jne print_asterisk
				mov word [es:di] , 0x072A
                pop di
				pop es
				pop cx
				pop ax
                jmp far [cs:oldisr]				

print_asterisk : 
                mov word [es:di], 0x072A
				call DisplayScore
				

exit :          mov word [di_count], di
                pop di
				pop es
				pop ax
				ret


;----------------------------------------------------------------------------------------------------------------------------------------

kbisr :         push ax
                in al, 0x60
				
				cmp al, 0x48  ;Up
				jne case2
				mov word [Dir], 1
				jmp keyPressed
				
case2:			cmp al, 0x50 ;Down
                jne case3
				mov word [Dir], 2
				jmp keyPressed
				
				
case3 : 		cmp al, 0x4B ;Left
                jne case4
				mov word [Dir], 3
				jmp keyPressed
				
case4 :			cmp al, 0x4D ;Right
                jne end_kbisr
				mov word [Dir], 4

keyPressed :    mov word [TFlag], 1
				jmp end_kbisr

end_kbisr :     mov al, 0x20 
                out 0x20, al
				pop ax
				iret

;-----------------------------------------------------------------------------------------------------------------------------------------

timer :         inc word [cs:tick]
                cmp word [cs:tick], 18
                je Move
				jmp end1

Move :          cmp word [TFlag], 1
                jne initial_move
                call asteriskMove
				jmp resume

initial_move :  call Movement

resume :        mov word [cs:tick], 0
                
                
end1:           mov  al, 0x20 
                out  0x20, al 
                iret


;------------------------------------------------------------------------------------------------------------------------------------------
start :     call clrscr
			call SetBackground
			
			xor  ax, ax 
            mov  es, ax             ; point es to IVT base 
           
		    mov ax, [es:9*4]
			mov [oldisr], ax
			mov ax, [es:9*4+2]
			mov [oldisr+2], ax

   		    cli                     ; disable interrupts 
            mov word [es:9*4], kbisr
			mov [es:9*4+2], cs
			
			mov  word [es:8*4], timer; store offset at n*4 
            mov  [es:8*4+2], cs     ; store segment at n*4+2 
            sti                     ; enable interrupts 
 
            mov  dx, start          ; end of resident portion 
            add  dx, 15             ; round up to next para 
            mov  cl, 4 
            shr  dx, cl             ; number of paras  
            mov  ax, 0x3100         ; terminate and stay resident 
            int  0x21 

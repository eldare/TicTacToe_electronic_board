    org 8000h

    table db 9h dup(0)
    num db 0
    turn db 0h  ; 0fh-X(RED), 0f0h-O(GREEN)
    mone db 0   ; the number of cells been taken
    mode db 0   ; 0-p vs p, 1-p vs cpu


    org 100h
    jmp Start

Begin:
    PORT_ADDR_C equ 8000h
    SWI_ADDR_D  equ 8001h   ; PORT A
    LED_ADDR_D  equ 8003h   ; PORT C
    KEY_ADDR_D  equ 4000h
    LCD_ADDR_C  equ 4000h
    LCD_ADDR_D  equ 4001h
    MSG_LNT     equ 10h 

    key_list db '369#2580147*'
    table_value_x db 2h,6h,0ah,0eh,12h,16h,1ah,1eh,22h
    table_value_o db 1h,5h,9h,0dh,11h,15h,19h,1dh,21h

    txt1 db 020h,020h,020h,020h,020h,0a7h,0b6h,0a9h,0b0h,020h,031h,020h,0afh,0b7h,0a7h,0b9h  ;P1 won
    txt2 db 020h,020h,020h,020h,020h,0a7h,0b6h,0a9h,0b0h,020h,032h,020h,0afh,0b7h,0a7h,0b9h  ;P2 won
    txt3 db 020h,020h,020h,021h,0adh,0a9h,0a9h,0bah,0b1h,0a4h,020h,0b7h,0a7h,0b9h,0aeh,0a4h  ;game over
    txt4 db 020h,020h,031h,020h,0afh,0b7h,0a7h,0b9h,020h,0ach,0b9h,020h,0a5h,0b8h,0a5h,0bah  ;P1 next
    txt5 db 020h,020h,032h,020h,0afh,0b7h,0a7h,0b9h,020h,0ach,0b9h,020h,0a5h,0b8h,0a5h,0bah  ;P2 next
    txt6 db 020h,020h,020h,020h,020h,0a1h,0a5h,0b9h,020h,0a4h,0b1h,0b0h,020h,021h,0a0h,0ach  ;try again
    txt7 db 020h,020h,020h,020h,021h,0ach,0a9h,0a7h,0bah,0a4h,020h,0b7h,0a7h,0b9h,0aeh,0a4h  ;begin
    txt8 db 020h,020h,020h,0b9h,0a3h,0a7h,020h,0b7h,0a7h,0b9h,0aeh,0ach,020h,0b9h,0b7h,0a4h  ;restart
 
    wline1 db 0,1,2
    wline2 db 3,4,5
    wline3 db 6,7,8
    wline4 db 0,3,6
    wline5 db 1,4,7
    wline6 db 2,5,8
    wline7 db 0,4,8
    wline8 db 2,4,6

Start:     mov  ax, 0
           mov  ds, ax
           mov  ss, ax
           mov  sp, 80a0h
           call LDelay

           call PortInit             ; initiating the system
           call LDelay
           call LcdInit
           call LDelay
           call GameInit
           call LDelay

WaitKey:   call ChkKey               ; begin the game with player1
           mov  ax, 0
           mov  al, ds:num
           mov  di, ax
           call ChkTable
           cmp  bh, 1
           je   WaitKey
           call OutTable

           cmp  ds:mone, 9
           je   TotalEnd
; Check for a winner?
           cmp  ds:mone, 5
           jl   chk_mode
           call ChkWin
           cmp  ah, 0
           jne  ShowWin              ; there is a winner
           

chk_mode:  cmp  ds:mode, 0
           je   WaitKey              ; it's the player vs player mode
           call CpuPlay              ; it's the player vs cpu mode
           call OutTable

           cmp  ds:mone, 9
           je   TotalEnd
; Check for a winner?
           cmp  ds:mone, 5
           jl   WaitKey
           call ChkWin
           cmp  ah, 0
           jne  ShowWin              ; there is a winner
           jmp  WaitKey              ; there is no winner, and it's player's turn

; restart?!
TotalEnd:  lea  si, cs:txt8
           call LcdMsg
           call LDelay
           call ChkKey
           call GameInit
           call LDelay
           jmp  WaitKey



;--------------------------------------------------------;
; Name:   ShowWin.                                       ;
; Type:   Non-return lable                               ;
; Task:   Displays the winner on LCD, and ends the game. ;
; Input:  AH - winner's value [0fh-P1, 0f0h-P2].         ;
; Return: None.                                          ;
;--------------------------------------------------------;
ShowWin:   push si                 
           push cx
           cmp  ah, 0fh
           jne  msg_win2
msg_win1:  lea  si, cs:txt1          ; SI receives the first address of P1's message
           jmp  swin_end
msg_win2:  lea  si, cs:txt2          ; SI receives the first address of P2's message
swin_end:  call LcdMsg               ; display on the LCD the selected message
           mov  cx, 0ffh
swin_dly:  call LedDelay
           call OutTable
           loop swin_dly
           pop  cx
           pop  si
           jmp  TotalEnd             ; jump to the last point of the game - Game Over.


;--------------------------------------------------------;
; Name:   ChkWin.                                        ;
; Type:   Procedure.                                     ;
; Task:   Detects if there is a winner.                  ;
; Input:  None.                                          ;
; Return: AH - winner's value [0-none, 0fh-P1, 0f0h-P2]. ;
;--------------------------------------------------------;
ChkWin:    push si
           lea  si, cs:wline1
           call LineWin
           cmp  al, 1                ; AL=1 -> there's a win!
           je   chkw_end

           lea  si, cs:wline2
           call LineWin
           cmp  al, 1
           je   chkw_end
  
           lea  si, cs:wline3
           call LineWin
           cmp  al, 1
           je   chkw_end

           lea  si, cs:wline4
           call LineWin
           cmp  al, 1
           je   chkw_end

           lea  si, cs:wline5
           call LineWin
           cmp  al, 1
           je   chkw_end

           lea  si, cs:wline6
           call LineWin
           cmp  al, 1
           je   chkw_end

           lea  si, cs:wline7
           call LineWin
           cmp  al, 1
           je   chkw_end

           lea  si, cs:wline8
           call LineWin
           cmp  al, 1
           je   chkw_end

           mov  ah, 0                ; AL=0 -> there's no win!
chkw_end:  pop  si
           ret


;--------------------------------------------------------;
; Name:   LineWin.                                       ;
; Type:   Procedure.                                     ;
; Task:   An assistant procedure for ChkWin Procedure,   ;
;         in order to check if there is a winner in the  ;
;         checked "line".                                ;
; Input:  SI - the first address of the "line".          ;
; Return: AH - player's value [0fh-P1, 0f0h-P2].         ;
;         AL - was there a win?! [1-yes, 0-no].          ;
;--------------------------------------------------------;
LineWin:   mov  al, 0                ; not win!
           mov  bh, 0                ; initting BX
           mov  bl, cs:[si]
           mov  ah, ds:table[bx]
           mov  bl, cs:[si+1]
           cmp  ah, ds:table[bx]
           jne  lw_end
           mov  bl, cs:[si+2]
           cmp  ah, ds:table[bx]
           jne  lw_end
           mov  al, 1                ; win!
lw_end:    ret


;---------------------------------------------------------;
; Name:   PortInit.                                       ;
; Type:   Procedure.                                      ;
; Task:   Sets the status of each and every port in 8155. ;
; Input:  None.                                           ;
; Return: None.                                           ;
;---------------------------------------------------------;
PortInit:  push dx
           push ax
           mov  dx, PORT_ADDR_C
           mov  al, 0cch              ; The Control word for the 
           out  dx, al                ; Ports: A=IN, B=IN, C=OUT
           call LDelay                ; CCh = 11001100b
           pop  ax
           pop  dx
           ret


;---------------------------------------------------------;
; Name:   GameInit.                                       ;
; Type:   Procedure.                                      ;
; Task:   Sets all the important variables of the game.   ;
; Input:  None.                                           ;
; Return: None.                                           ;
;---------------------------------------------------------;
GameInit:  push si
           push dx
           push ax
           call ClrTable
           mov  ds:turn, 0fh
           mov  ds:mone, 0
           mov  ds:num, 0
           mov  dx, SWI_ADDR_D        ; get the play mode
           in   al, dx
           and  al, 1h
           mov  ds:mode, al
           lea  si, cs:txt7           ; display the 'start' message
           call LcdMsg
           pop  ax
           pop  dx
           pop  si
           ret


;-------------------------------------------------------;
; Name:   ClrTable.                                     ;
; Type:   Procedure.                                    ;
; Task:   Clears the LEDs display and table.            ;
; Input:  None.                                         ;
; Return: None.                                         ;
;-------------------------------------------------------;
ClrTable:  push si           
           push dx
           push ax
           mov  si, 0
clr_loop:  mov  ds:table[si], 0
           inc  si
           cmp  si, 9                ; (loop for 9 times)
           jl   clr_loop             ; and input zero into every cell.
           mov  dx, LED_ADDR_D       ; up-date the LEDs display.
           mov  al, 0
           out  dx, al
           pop  ax
           pop  dx
           pop  si
           ret


;--------------------------------------------------------;
; Name:   LcdInit.                                       ;
; Type:   Procedure.                                     ;
; Task:   Clears the LCD display.                        ;
; Input:  None.                                          ;
; Return: None.                                          ;
;--------------------------------------------------------;
LcdInit:   push dx
           push ax
           mov  dx, LCD_ADDR_C
           mov  al, 1h               ; Clear LCD
           out  dx, al
           call SDelay
           mov  al, 0ch              ; LCD Display On, and no Cursor
           out  dx, al
           call SDelay
           mov  al, 6h               ; LCD Entery Mode Set
           out  dx, al
           call SDelay
           mov  al, 3bh              ; LCD Function Set
           out  dx, al
           call SDelay
           mov  al, 80h              ; LCD Line Set
           out  dx, al
           call SDelay
           pop  ax
           pop  dx
           ret


;--------------------------------------------------------;
; Name:   LcdMsg.                                        ;
; Type:   Procedure.                                     ;
; Task:   Displays a message on the LCD.                 ;
; InPut:  SI - first addr of the displayed message.      ;
; Return: None.                                          ;
;--------------------------------------------------------;
LcdMsg:    push ax
           push dx
           push cx
           mov  dx, LCD_ADDR_C
           mov  al, 1h               ; Clear LCD
           out  dx, al
           call SDelay
           mov  dx, LCD_ADDR_D
           mov  cx, MSG_LNT          ; Loop CX times - CX chars in a LCD message
lrep:      mov  al, cs:[si]
           inc  si
           out  dx, al               ; Send a char to the LCD
           call SDelay
           loop lrep
           pop  cx
           pop  dx
           pop  ax
           ret


;--------------------------------------------------------;
; Name:   ChkKey.                                        ;
; Type:   Procedure.                                     ;
; Task:   Loops until a key is pressed.                  ;
; InPut:  None.                                          ;
; Return: DS:NUM - the pressed key.                      ;
;--------------------------------------------------------;
ChkKey:    push ax
           push bx
           push dx
           mov  dx, KEY_ADDR_D
kchk:      call OutTable             ; we loop forever, until a key has been
           in   al, dx               ; pressed but while we wait for a key, with 
           mov  bl, al               ; every loop we display info on the LEDs
           and  al, 10h          
           je   kchk
           mov  ah, 00h
           mov  al, bl
           and  al, 0fh
           mov  si, ax
           mov  al, cs:key_list[si]  ; we give the pressed key the right value
           mov  ds:num, al
           pop  dx
           pop  bx
           pop  ax
           ret


;-------------------------------------------------------;
; Name:   ChkTable.                                     ;
; Task:   Checks if the choisen cell is already taken.  ;
; Input:  DI - the decoded number of the cell.          ;
; Return: BH - 0: cell empty.  1: cell taken.           ;
;-------------------------------------------------------;
ChkTable:  push si
           push ax           
           sub  di, 30h
           cmp  di, 0
           je   chk_msg3
           cmp  ds:table[di-1], 0    ; check if the cell is empty
           jne  chk_msg3
           mov  bh, ds:turn          ; embedding the player's 'name' in the cell
           mov  ds:table[di-1], bh
           mov  bh, ds:turn          ; switching the turn to the second player
           not  bh
           mov  ds:turn, bh          
          
           mov  al, ds:mone
           inc  al
           mov  ds:mone, al
           cmp  bh, 0fh              ; check who's next?!
           jne  chk_msg2
chk_msg1:  lea  si, cs:txt4          ; LCD display - "P1 is next"
           mov  bh, 0                ; the cell is taken - value
           jmp  end_chk
chk_msg2:  lea  si, cs:txt5          ; LCD display - "P2 is next"
           mov  bh, 0                ; the cell is taken - value
           jmp  end_chk
chk_msg3:  lea  si, cs:txt6          ; LCD display - "Try again"
           mov  bh, 1                ; the cell was empty - value
end_chk:   call LcdMsg
           call LDelay
           pop  ax
           pop  si
           ret

;-------------------------------------------------------;
; Name:   OutTable.                                     ;
; Type:   Procedure.                                    ;
; Task:   displays the table on the LEDs.               ;
; Input:  None.                                         ;
; Return: None.                                         ;
;-------------------------------------------------------;
OutTable:  push dx
           push si
           push bx
           push ax
           mov  dx, LED_ADDR_D
           mov  si, 0
cell_num:  mov  bl, ds:table[si]     ; scans for which LED to light, and it's color.
           cmp  bl, 0
           je   table_end
           cmp  bl, 0fh
           je   value_x
           cmp  bl, 0f0h
           je   value_o

value_x:   mov  al, cs:table_value_x[si]
           jmp  table_out
value_o:   mov  al, cs:table_value_o[si]
    
table_out: out  dx, al               ; sends the value of which LED to light, and it's color.
           call LedDelay
table_end: inc  si
           cmp  si, 9
           jl   cell_num
           pop  ax
           pop  bx
           pop  si
           pop  dx
           ret


;--------------------------------------------------------;
; Name:   CpuPlay.                                       ;
; Type:   Procedure.                                     ;
; Task:   A step the CPU would like to take.             ;
; Input:  None.                                          ;
; Return: None.                                          ;
;--------------------------------------------------------;
CpuPlay:   push si
           push di
           push dx
           push cx
           mov  cx, 10
cpu_dly:   call LedDelay
           call OutTable
           loop cpu_dly
           lea  si, cs:wline1        ; get the first address.
           call FindStep             ; try to decide on the next step.
           cmp  dh, 0                ; check if a step has been found.
           je   cpu_end
    
           lea  si, cs:wline2
           call FindStep
           cmp  dh, 0
           je   cpu_end

           lea  si, cs:wline3
           call FindStep
           cmp  dh, 0
           je   cpu_end

           lea  si, cs:wline4
           call FindStep
           cmp  dh, 0
           je   cpu_end
           
           lea  si, cs:wline5
           call FindStep
           cmp  dh, 0
           je   cpu_end
    
           lea  si, cs:wline6
           call FindStep
           cmp  dh, 0
           je   cpu_end

           lea  si, cs:wline7
           call FindStep
           cmp  dh, 0
           je   cpu_end

           lea  si, cs:wline8
           call FindStep

cpu_end:   mov  dl, 0f0h             ; place the cpu value
           mov  ds:table[di], dl     ; in the Table.
           mov  dl, 0fh              ; change turn, back to player
           mov  ds:turn, dl
           mov  dl, ds:mone          ; inc the mone
           inc  dl
           mov  ds:mone, dl
           lea  si, cs:txt4          ; Displays: player1 is next
           call LcdMsg
           call LDelay
           pop  cx
           pop  dx
           pop  di
           pop  si
           ret


;--------------------------------------------------------;
; Name:   FindStep.                                      ;
; Type:   Procedure.                                     ;
; Task:   An assistant procedure for CpuPlay Procedure,  ;
;         in order to detect a step.                     ;
; Input:  SI - the first address of the "line".          ;
; Return: DI - number of the chosen Table cell.          ;
;         DH - 0-Step found, 1-Step not found.           ;
;--------------------------------------------------------;
FindStep:  push ax
           push cx
           mov  dh, 1                ; step not found
           mov  cx, 3                ; 3 cells in a "line"
           mov  ah, 0                ; 0fh counter
           mov  bh, 0                ; initting BX
fs_again:  mov  bl, cs:[si]
           mov  al, ds:table[bx]
           cmp  al, 0                ; checks if the Table's
           je   fs_empty             ; cell contains zero.
           cmp  al, 0fh
           je   fs_count
           jmp  fs_end

fs_count:  inc  ah
           jmp  fs_loop
fs_empty:  mov  di, bx               ; marks an empty cell
fs_loop:   inc  si
           loop fs_again

           cmp  ah, 2
           jne  fs_end
           mov  dh, 0
fs_end:    pop  cx
           pop  ax
           ret


;-------------------------------------------------------;
; Name:   LedDelay.                                     ;
; Type:   Procedure.                                    ;
; Task:   A delay between Leds.                         ;
; Input:  None.                                         ;
; Return: None.                                         ;
;-------------------------------------------------------;
LedDelay:  push cx
           mov  cx, 0100h
lldly:     loop lldly                ; Loops while CX > 0
           pop  cx
           ret


;--------------------------------------------------------;
; Name:   SDelay.                                        ;
; Type:   Procedure.                                     ;
; Task:   A short delay.                                 ;
; InPut:  None.                                          ;
; Return: None.                                          ;
;--------------------------------------------------------;
SDelay:    push cx
           mov  cx, 01ffh
sdly:      loop sdly                 ; loop while CX > 0
           pop  cx
           ret


;--------------------------------------------------------;
; Name:   LDelay.                                        ;
; Type:   Procedure.                                     ;
; Task:   A large delay.                                 ;
; InPut:  None.                                          ;
; Return: None.                                          ;
;--------------------------------------------------------;
LDelay:    push cx
           mov  cx, 0ffffh
ldly:      loop ldly                 ; loop while CX > 0
           pop  cx
           ret

END

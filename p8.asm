;Highway Havoc Game intelx86 Assembly Language Code 
[org 0x0100]                  ;origin point of the program

section .data                 ;data section for variable declarations

Car:        dw 0x00DB         ;player One's car color
carx:       dw 29             ;player One's initial x-coordinate
cary:       dw 18             ;player One's initial y-coordinate

Car1:       dw 0x00DB         ;player Two's car color
car1x:      dw 49             ;player Two's initial x-coordinate
car1y:      dw 18             ;player Two's initial y-coordinate

reward:     db 0x0701         ;rewards color and points
rewardx:    dw 0              ;reward's x-coordinate
rewardy:    dw 1              ;reward's y-coordinate

obstacle:   dw 0x03B0         ;explosives color
obsy:       dw 1              ;explosives y-coordinate
obsx:       dw 22             ;explosives x-coordinate

HEIGHT:     dw 22             ;board Height
WIDTH:      dw 40             ;board Width

checker:    dw 0, 0, 0,0, 0   ;checker for borders of cars to prevent breaking borders

seed:       dd 0              ;to generate random number or store the time stamp
random:     dw 0              ;to store the random number

score:      dw 0              ;player one's score
score1:     dw 0              ;player two's score

names:      db 'Muhammad Abdul Hanan Nadeem',0         ;player one's name
name1:      db 'Syed Muhammad Zafeer', 0               ;player two's name

game_title: db 'Highway Havoc', 0                      ;game title

player1_score_str: db 'Player_1_Score  ', 0            ;player one's score label
player2_score_str: db 'Player_2_Score  ', 0            ;player two's score label
player1_score_str1: db 'Player_1 Won!  ', 0            ;label of winning of player one  
player2_score_str1: db 'Player_2 Won!  ', 0            ;label of winning of player two 
str12: db 'No one Wins', 0                             ;label of no one wins  

delay:
    push ax          ;save the value of the 'ax' register on the stack
    push cx          ;save the value of the 'cx' register on the stack

    mov ax, 0xffff   ;load the value 0xffff into the 'ax' register
    mov cx, ax       ;copy the value of AX into the 'cx' register

    ;at this point, the AX and CX registers both hold the value 0xffff,
    ;which is often used as a large counter for implementing a delay loop.
    ;the delay subroutine is now complete, and execution will continue from where it was called.
Y:
    loop Y       ;this is a loop labeled 'Y' using the loop instruction
                 ;it decrements the 'cx' register and jumps to the 'Y' label until 'cx' becomes zero

    pop cx       ;restore the original value of the 'cx' register from the stack
    pop ax       ;restore the original value of the 'ax' register from the stack

    ret          ;return from the subroutine

;the purpose of this code appears to be implementing a delay loop using the loop instruction.
;the loop instruction decrements the 'cx' register and jumps to the specified label until 'cx' becomes zero.
;the delay duration is determined by the initial value loaded into the 'cx' register before the loop.

;the delay loop is intended to be used as a subroutine, and after the delay, the original values of
;'cx' and 'ax' are restored from the stack, and the subroutine returns.

drawNumber:
    ;input: AX - Number to be displayed (assumes a two-digit number)
    ;output: Displays the number on the screen

    push ax         ;save the value of AX on the stack
    push bx         ;save the value of BX on the stack
    push cx         ;save the value of CX on the stack
    push dx         ;save the value of DX on the stack
    push di         ;save the value of DI on the stack
    push si         ;save the value of SI on the stack

    mov cx, 2       ;set loop counter to 2 (for two digits)

    ;loop to extract tens and units place digits
    kli:
        mov bx, 10   ;set divisor for extracting tens place
        mov dx, 0    ;initialize tens place

        div bx       ;AX / 10 -> Quotient in AX, Remainder in DX
        add dl, '0'  ;convert the remainder to ASCII and save the tens place
        push dx      ;push the ASCII value of the tens place onto the stack
        loop kli     ;repeat the loop for the units place

    mov cx, 2        ;reset loop counter to 2 for printing
    ;loop to print the digits
    kli1:
        mov ah, 0x0e  ;set the BIOS function for teletype output
        pop dx        ;pop the ASCII value of the digit from the stack
        mov al, dl    ;move the ASCII value to AL
        int 0x10      ;invoke BIOS interrupt to print the character
        loop kli1     ; repeat the loop for the second digit

    pop si           ;restore the value of SI from the stack
    pop di           ;restore the value of DI from the stack
    pop dx           ;restore the value of DX from the stack
    pop cx           ;restore the value of CX from the stack
    pop bx           ;restore the value of BX from the stack
    pop ax           ;restore the value of AX from the stack

    ret              ;return from the subroutine

drawScores:
    pushad          ;save all general-purpose registers on the stack

    ;set the cursor position for player 1 score label
    mov dh, 2       ;set the row (Y position)
    mov dl, 3       ;set the column (X position)
    mov ah, 0x02    ;function: Set Cursor Position
    int 0x10

    mov ah, 0x0e    ;function: Teletype Output
    mov bx, 0x0007  ;video attribute (color)
    mov si, player1_score_str  ;pointer to the null-terminated string
    .next_char2:
        lodsb      ;load the next character from the string into AL
        cmp al, 0  ;check if the character is null (end of string)
        jz .done89 ;if null, exit the loop
        mov cx, 1  ;set the character count to 1
        int 0x10   ;BIOS interrupt to print the character
        jmp .next_char2  ;jump to the next character in the string
    .done89:

    ;set the cursor position for player 1 score value
    mov dh, 3       ;set the row (Y position)
    mov dl, 3       ;set the column (X position)
    mov ah, 0x02    ;function: Set Cursor Position
    int 0x10

    ;display player 1 score
    mov ax, [score]   ;load player 1 score from memory
    call drawNumber   ;call drawNumber subroutine to display the score

    ;set the cursor position for player 2 score label
    mov dh, 2       ;set the row (Y position)
    mov dl, 63      ;set the column (X position)
    mov ah, 0x02    ;function: Set Cursor Position
    int 0x10

    mov ah, 0x0e    ;function: Teletype Output
    mov bx, 0x0007  ;video attribute (color)
    mov si, player2_score_str  ;pointer to the null-terminated string
    .next_char1:
        lodsb      ;load the next character from the string into AL
        cmp al, 0  ;check if the character is null (end of string)
        jz .done39 ;if null, exit the loop
        mov cx, 1  ;set the character count to 1
        int 0x10   ;BIOS interrupt to print the character
        jmp .next_char1  ;jump to the next character in the string
    .done39:

    ;set the cursor position for player 2 score value
    mov dh, 3       ;set the row (Y position)
    mov dl, 63      ;set the column (X position)
    mov ah, 0x02    ;function: Set Cursor Position
    int 0x10

    ;display player 2 score
    mov ax, [score1]  ;load player 2 score from memory
    call drawNumber   ;call drawNumber subroutine to display the score

    popad           ;restore all general-purpose registers from the stack
    ret             ;return from the subroutine

;drawString subroutine: Draws a null-terminated string at the specified position
;save all general-purpose registers on the stack
drawString:
    pusha 

    ;set the cursor position for the game title
	mov dh, 9        ;set the row (Y position)
    mov dl, 33       ;set the column (X position)

    ;set the cursor position using BIOS interrupt 0x10
    mov ah, 0x02     ;function: Set Cursor Position
    int 0x10

    mov ah, 0x0e        ;function: Teletype Output
	mov bx, 0x0007      ;video attribute (color)
	mov si, game_title  ;pointer to the null-terminated string

	;loop to print characters from the string until null character (end of string)
	.next_char2:
		lodsb               ;load the next character from the string into AL
		cmp al, 0           ;check if the character is null (end of string)
		jz .done2           ;if null, exit the loop
		mov cx, 1           ;set the character count to 1
		int 0x10            ;BIOS interrupt to print the character
		jmp .next_char2     ;jump to the next character in the string

    .done2:

	;set the cursor position for the second string (name1)
	mov dh, 11       ;set the row (Y position)
    mov dl, 30       ;set the column (X position)

    ;set the cursor position using BIOS interrupt 0x10
    mov ah, 0x02     ;function: Set Cursor Position
    int 0x10

    mov ah, 0x0e     ;function: Teletype Output
	mov bx, 0x0007   ;video attribute (color)
	mov si, name1    ;pointer to the null-terminated string

	;loop to print characters from the string until null character (end of string)
	.next_char:
		lodsb           ;load the next character from the string into AL
		cmp al, 0       ;check if the character is null (end of string)
		jz .done1       ;if null, exit the loop
    mov cx, 1           ;set the character count to 1
		int 0x10        ;BIOS interrupt to print the character
		jmp .next_char  ;jump to the next character in the string

    .done1:

	;set the cursor position for the third string (names)
	mov dh, 13       ;set the row (Y position)
    mov dl, 26       ;set the column (X position)

    ;set the cursor position using BIOS interrupt 0x10
    mov ah, 0x02     ;function: Set Cursor Position
    int 0x10

    mov ah, 0x0e     ;function: Teletype Output
	mov bx, 0x0007   ;video attribute (color)
	mov si, names    ;pointer to the null-terminated string

	;loop to print characters from the string until null character (end of string)
	.next_char1:
		lodsb               ;load the next character from the string into AL
		cmp al, 0           ;check if the character is null (end of string)
		jz .done            ;if null, exit the loop
		mov cx, 1           ;set the character count to 1
		int 0x10            ;BIOS interrupt to print the character
		jmp .next_char1     ;jump to the next character in the string

	.done:

    ;restore all general-purpose registers from the stack
    popa

    ;return from the subroutine
    ret

Board:
    ;save registers on the stack
    push ax
    push bx
    push cx
    push es
    push di

    ;set ES to the video memory segment (0xb800)
    mov ax, 0xb800
    mov es, ax

    ;initialize DI to the starting position in video memory
    mov di, 42
    ;get the width from the variable WIDTH
    mov cx, [WIDTH]            ;run the loop for WIDTH times
    dec cx                     ;decrease CX by 1
    ;set AX to the ASCII code for borders
    mov ax, 0x07cd
    ;set the left corners of the board
    mov word [es:di - 2], 0x07c9 ;set the specific ASCII for left corners

    ;clear direction flag to move forward in memory operations
    cld
    ;repeat the operation (stosw) CX times, storing AX at ES:DI
    rep stosw

    ;set the right corner of the board
    mov word [es:di - 2], 0x07bb ;set the specific ASCII for right corner

    ;set DI to the starting position for vertical borders
    mov di, 40
    ;calculate the total number of bytes to move for vertical borders
    mov bx, [WIDTH]            ;get WIDTH
    dec bx                     ;decrease BX by 1
    shl bx, 1                  ;multiply BX by 2 (since each character is 2 bytes)
    ;get the height from the variable HEIGHT
    mov cx, [HEIGHT]
    ;set AX to the ASCII code for vertical borders
    mov ax, 0x0720


; side_board subroutine: Draws side borders of the board
side_board:
    add di, 160                 ; Move to the next row
    mov word [es:di], 0x07ba    ; Draw left border at the start of the board
    mov word [es:di+bx], 0x07ba ; Draw right border at the other end
    loop side_board             ; Repeat the process

    ; Centre Line
    push di
    mov di, 78                   ; Move to the center of the screen
    mov bx, [WIDTH]              ; Get the width
    dec bx                       ; Decrease BX by 1
    shl bx, 1                    ; Multiply BX by 2 (since each character is 2 bytes)
    mov cx, [HEIGHT]             ; Get the height
    mov ax, 0x0720               ; Set ASCII for vertical borders

side_board1:
    add di, 160                  ; Move to the next row
    mov word [es:di], 0x07ba    ; Draw a vertical border
    loop side_board1             ; Repeat the process

    pop di

    ; The last line
    mov cx, [WIDTH]              ; Get the width
    dec cx                       ; Decrease CX by 1
    mov ax, 0x07cd               ; Set ASCII for borders
    mov word [es:di], 0x07c8     ; Draw top left corner
    add di, 2
    cld
    rep stosw                    ; Draw the horizontal line
    mov word [es:di-2], 0x07bc   ; Draw top right corner

    pop di
    pop es
    pop cx
    pop bx
    pop ax
    ret

; clear_board_screen subroutine: Clears the board space on the screen
clear_board_screen:
    push ax
    push cx
    push es
    push di
    mov di, 42                   ; Set DI to the starting position in video memory
    mov cx, [HEIGHT]             ; Get the height
    mov ax, 0xb800               ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 0x0720               ; Set ASCII for background space
    cld

cbsloop1:
    push cx
        mov cx, [WIDTH]          ; Get the width
        dec cx                   ; Decrease CX by 1
        push di
        rep stosw                ; Clear a row of the board
        pop di
        add di, 160              ; Move to the next row
    pop cx
    loop cbsloop1

    pop di
    pop es
    pop cx
    pop ax
    ret

; cls subroutine: Clears the entire screen with the ASCII of background space
cls:
    push cx
    push di
    push ax
    push es

    mov cx, 2000                 ; Set CX to the total number of characters on the screen
    mov di, 0                    ; Set DI to the starting position in video memory
    mov ax, 0xb800               ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 0x07b0               ; Set ASCII for background space
    cld
loop1:
    stosw                        ; Clear a character on the screen
    loop loop1

    pop es
    pop ax
    pop di
    pop cx
    ret

; undraw_obs subroutine: Undraws the obstacle on the screen
UndrawObs:
    pushad                       ; Save all general-purpose registers on the stack
    mov ax, 0xb800               ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 80                   ; Multiply obsy by 80 (screen width)
    mul byte [obsy]
    add ax, [obsx]                ; Add obsx to the calculated position
    shl ax, 1                    ; Multiply AX by 2 (since each character is 2 bytes)
    mov di, ax
    cmp word [es:di], 0x0EDB      ; Check if the character is the obstacle
    je io                         ; If yes, jump to io
    cmp word [es:di+2], 0x0EDB
    je io
    cmp word [es:di], 0x01DB
    je io
    cmp word [es:di+2], 0x01DB
    je io
    mov ax, 0x0720                ; Set ASCII for background space
    mov cx, 2                     ; For the first two characters
    push di
    rep stosw                    ; Undraw the obstacle
    pop di

    add di, 160                   ; Move to the next row
    cmp word [es:di], 0x0EDB      ; Check if the character is the obstacle
    je io
    cmp word [es:di+2], 0x0EDB
    je io
    cmp word [es:di], 0x01DB
    je io
    cmp word [es:di+2], 0x01DB
    je io
    mov cx, 2                     ; For the second set of characters
    push di
    rep stosw                    ; Undraw the obstacle
    pop di
    jmp lpp                      ; Jump to lpp

io:
    call drawScores            ; Call draw_scores subroutine
    call Winning                 ; Call winning subroutine
    mov ax, 0x4c00                ; Terminate the program
    int 21h

lpp:
    popad                        ; Restore all general-purpose registers from the stack
    ret                           ; Return from the subroutine

;undraw_car1 subroutine: Undraws the car1 on the screen
UndrawCar1:
    ;For car1
    ; Save all general-purpose registers on the stack
    pushad
    mov ax, 0xb800          ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 80              ; Multiply car1y by 80 (screen width)
    mul byte [car1y]
    add ax, [car1x]          ; Add car1x to the calculated position
    shl ax, 1               ; Multiply AX by 2 (since each character is 2 bytes)
    mov di, ax
    mov ax, 0x0720          ; Set ASCII for background space

    mov word [es:di], ax     ; Head of the car1
    add di, 158              ; Move to the body of the car1
    mov cx, 3                ; Set CX to 3 for the body
    push di
    rep stosw               ; Undraw the body of the car1
    pop di

    add di, 160              ; Move to the next row for the body
    mov cx, 3                ; Set CX to 3 for the body
    push di
    rep stosw               ; Undraw the body of the car1
    pop di

    add di, 162              ; Move to the tail of the car1
    mov word [es:di], ax     ; Tail of the car1
    ; Restore all general-purpose registers from the stack
    popad
    ret                      ; Return from the subroutine

; undraw_car subroutine: Undraws the car on the screen
UndrawCar:
    ;for car 
    ; Save all general-purpose registers on the stack
    pushad
    mov ax, 0xb800          ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 80              ; Multiply cary by 80 (screen width)
    mul byte [cary]
    add ax, [carx]           ; Add carx to the calculated position
    shl ax, 1               ; Multiply AX by 2 (since each character is 2 bytes)
    mov di, ax
    mov ax, 0x0720          ; Set ASCII for background space

    mov word [es:di], ax     ; Head of the car
    add di, 158              ; Move to the body of the car
    mov cx, 3                ; Set CX to 3 for the body
    push di
    rep stosw               ; Undraw the body of the car
    pop di

    add di, 160              ; Move to the next row for the body
    mov cx, 3                ; Set CX to 3 for the body
    push di
    rep stosw               ; Undraw the body of the car
    pop di

    add di, 162              ; Move to the tail of the car
    mov word [es:di], ax     ; Tail of the car
    ; Restore all general-purpose registers from the stack
    popad
    ret                      ; Return from the subroutine
; drawObs subroutine: Draws the obstacle on the screen
drawObs:
    ; Save base pointer, general-purpose registers, and stack pointer
    push bp
    mov bp, sp
    push ax
    push cx
    push di
    push es

    mov ax, 0xb800          ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 80              ; Multiply obsy by 80 (screen width)
    mul byte [obsy]
    add ax, [obsx]           ; Add obsx to the calculated position
    shl ax, 1               ; Multiply AX by 2 (since each character is 2 bytes)
    mov di, ax

    ; Comparison to check if the character is an exploding car
    cmp word [es:di], 0x0EDB
    je io1
    cmp word [es:di+2], 0x0EDB
    je io1
    cmp word [es:di], 0x01DB
    je io1
    cmp word [es:di+2], 0x01DB
    je io1

    mov ax, [bp+4]          ; Get the color attribute from the stack
    mov ah, 0x04            ; Set AH to the BIOS function for "Write Character and Attribute at Cursor Position"
    mov cx, 2               ; Set CX to 2 for the length of the obstacle
    push di
    rep stosw               ; Draw the obstacle
    pop di

    add di, 160             ; Move to the next row
    ; Comparison to check if the character is an exploding car
    cmp word [es:di], 0x0EDB
    je io1
    cmp word [es:di+2], 0x0EDB
    je io1
    cmp word [es:di], 0x01DB
    je io1
    cmp word [es:di+2], 0x01DB
    je io1

    mov cx, 2               ; Set CX to 2 for the length of the obstacle
    push di
    rep stosw               ; Draw the obstacle on the next row
    pop di
    jmp ll                  ; Jump to the label ll

; io1 subroutine: Handles actions when the obstacle is an exploding car
io1:
    ; Call drawScores subroutine
    call drawScores
    ; Call Winning subroutine
    call Winning
    mov ax, 0x4c00           ; Set AH to the BIOS function for program termination
    int 21h                   ; Interrupt 21h - terminate the program

; ll subroutine: Common cleanup code after drawing the obstacle
ll:
    pop es                   ; Restore ES from the stack
    pop di                   ; Restore DI from the stack
    pop cx                   ; Restore CX from the stack
    pop ax                   ; Restore AX from the stack
    pop bp                   ; Restore BP from the stack
    ret 2                    ; Return from the subroutine with a 2-byte pop


; drawCar1 subroutine: Draws the car1 on the screen
drawCar1:
    push bp                  ; Save base pointer on the stack
    mov bp, sp               ; Set base pointer to stack pointer
    push ax                  ; Save AX register on the stack
    push cx                  ; Save CX register on the stack
    push di                  ; Save DI register on the stack
    push es                  ; Save ES register on the stack

    mov ax, 0xb800           ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 80               ; Multiply car1y by 80 (screen width)
    mul byte [car1y]
    add ax, [car1x]           ; Add car1x to the calculated position
    shl ax, 1               ; Multiply AX by 2 (since each character is 2 bytes)
    mov di, ax
    mov ax, [bp+4]           ; Get the color attribute from the stack
    mov ah, 0x01             ; Set AH to the BIOS function for "Write Character at Cursor Position"

    ; Draw head
    mov word [es:di], ax
    cmp al, 0x20             ; Compare the low byte of AX with 0x20 (empty space)
    jne skip1                ; If not equal, jump to skip1
    mov word [es:di], ax     ; Draw the head of the car1

skip1:
    ; Draw body
    add di, 158              ; Move to the body of the car1
    mov cx, 3                ; Set CX to 3 for the length of the body
    push di
    rep stosw               ; Draw the body of the car1
    pop di

    add di, 160              ; Move to the next row for the body
    mov cx, 3                ; Set CX to 3 for the length of the body
    push di
    rep stosw               ; Draw the body of the car1 on the next row
    pop di

    ; Draw tail
    add di, 162              ; Move to the tail of the car1
    mov word [es:di], ax     ; Draw the tail of the car1
    pop es                   ; Restore ES from the stack
    pop di                   ; Restore DI from the stack
    pop cx                   ; Restore CX from the stack
    pop ax                   ; Restore AX from the stack
    pop bp                   ; Restore BP from the stack
    ret 2                    ; Return from the subroutine with a 2-byte pop


; drawCar subroutine: Draws the car on the screen
drawCar:
    ; Same as above
    push bp                  ; Save base pointer on the stack
    mov bp, sp               ; Set base pointer to stack pointer
    push ax                  ; Save AX register on the stack
    push cx                  ; Save CX register on the stack
    push di                  ; Save DI register on the stack
    push es                  ; Save ES register on the stack

    mov ax, 0xb800           ; Set ES to the video memory segment (0xb800)
    mov es, ax
    mov ax, 80               ; Multiply cary by 80 (screen width)
    mul byte [cary]
    add ax, [carx]           ; Add carx to the calculated position
    shl ax, 1               ; Multiply AX by 2 (since each character is 2 bytes)
    mov di, ax
    mov ax, [bp+4]           ; Get the color attribute from the stack
    mov ah, 0x0e             ; Set AH to the BIOS function for "Write Character at Cursor Position"

    mov word [es:di], ax     ; Draw the head of the car
    cmp al, 0x20             ; Compare the low byte of AX with 0x20 (empty space)
    jne skip                 ; If not equal, jump to skip
    mov word [es:di], ax     ; Draw the head of the car

skip:
    ; Move to the body of the car
    add di, 158              ; Move to the body of the car
    mov cx, 3                ; Set CX to 3 for the length of the body
    push di
    rep stosw               ; Draw the body of the car
    pop di

    ; Move to the next row for the body
    add di, 160              ; Move to the next row for the body
    mov cx, 3                ; Set CX to 3 for the length of the body
    push di
    rep stosw               ; Draw the body of the car on the next row
    pop di

    ; Move to the tail of the car
    add di, 162              ; Move to the tail of the car
    mov word [es:di], ax     ; Draw the tail of the car
    pop es                   ; Restore ES from the stack
    pop di                   ; Restore DI from the stack
    pop cx                   ; Restore CX from the stack
    pop ax                   ; Restore AX from the stack
    pop bp                   ; Restore BP from the stack
    ret 2                    ; Return from the subroutine with a 2-byte pop

;Taken help from chat-gpt for random number
; genE subroutine: Generates a random number
genE:
    ; Save all general-purpose registers on the stack
    pushad
    rdtsc                    ; Read Time Stamp Counter (TSC)
    mov word [seed], ax      ; Store the low 16 bits of TSC in seed
    mov bx, 36               ; Set divisor for generating a random number between 22 and 57
    div bx                   ; Divide the value in DX:AX by 36
    add dx, 22               ; Add 22 to the result (generating a random number between 22 and 57)
    mov word [random], dx    ; Store the random number in the random variable
    ; Restore all general-purpose registers from the stack
    popad
    ret                      ; Return from the subroutine


; drawE subroutine: Uses video interrupt to draw an object (E)
drawE:
    ; Save all general-purpose registers on the stack
    pushad
    mov ah, 0x13             ; Set AH to the BIOS video interrupt function for graphics mode
    mov al, 1                ; Set AL to 1 for writing pixels
    mov dh, [rewardy]        ; Load the Y position of the object (E)


; R subroutine: Generates a random number, compares it, and draws an object (E) on the screen
R:
    ; Call genE subroutine to generate a random number
    call genE
    cmp word [random], 39    ; Compare the generated random number with 39
    je R                     ; If equal, jump to R (retry)
    mov dl, byte [random]    ; Move the low byte of the random number to DL
    mov bh, 0                ; Set BH to 0
    mov bl, 2                ; Set BL to 2 (color attribute for the object E)
    push cs                  ; Push code segment value onto the stack
    pop es                   ; Pop the value from the stack to ES
    mov bp, reward           ; Set BP to the address of the reward variable
    mov cx, 1                ; Set CX to 1 (number of pixels to write)
    int 0x10                 ; Call video interrupt to draw the object (E)
    popad                    ; Restore all general-purpose registers from the stack
    ret                      ; Return from the subroutine

;calculate positions
; calculate subroutine: Calculates a value based on parameters passed on the stack
calculate:
    push bp                  ; Save base pointer on the stack
    mov bp, sp               ; Set base pointer to stack pointer
    mov ax, 80               ; Move 80 to AX
    mul byte [bp+4]          ; Multiply AX by the value at offset 4 from the stack pointer
    add ax, word [bp+6]      ; Add the value at offset 6 from the stack pointer to AX
    shl ax, 1                ; Left shift AX by 1 (multiply by 2)
    pop bp                   ; Restore base pointer from the stack
    ret 4                    ; Return from the subroutine with a 4-byte pop

;check if the player is dead or not
; movE subroutine: Moves the object (E) to a specific line on the screen
movE:
    ; Save all general-purpose registers on the stack
    pushad
    ; Move until that line
    mov cx, 1680              ; Set CX to the number of iterations for moving
    mov di, 0                 ; Set DI to the starting offset in video memory
    mov ax, 0xb800            ; Move the video memory segment to AX
    mov es, ax                ; Set ES to the video memory segment


; F subroutine: Checks and processes collisions with the object (E) and updates scores
F:
    cmp word [es:di], 0x0201           ; Compare the content at the current position with the reward
    jne Q                             ; If not equal, jump to Q (skip processing)
        cmp word [es:di+160], 0x0EDB   ; Check collision with car
        je li                           ; If equal, jump to li (process reward taken by car)
        cmp word [es:di+160], 0x01DB   ; Check collision with car1
        je il                           ; If equal, jump to il (process reward taken by car1)
            call delay                   ; Call delay subroutine
            call delay                   ; Call delay subroutine again
            mov word [es:di], 0x0720    ; Move the object (E) to an empty space
            mov word [es:di+160], 0x0201 ; Move the reward to the next position
            cmp word [obsy], 20          ; Check if it's the last border, stop obstacle
            je Q
            call UndrawObs               ; Undraw to mimic movement
            add word [obsy], 1           ; Increment obstacle Y position
            push word [obstacle]
            call drawObs                 ; Draw the obstacle in the new position
            jmp Q
    li:
        mov word [es:di], 0x0720        ; Move the object (E) to an empty space
        add word [score], 1             ; Increment score for car
        jmp Q
    il:
        mov word [es:di], 0x0720        ; Move the object (E) to an empty space
        add word [score1], 1            ; Increment score for car1

; Q subroutine: Updates variables and continues the game loop
Q:
    add di, 2                   ; Move to the next position in video memory
    add word [rewardy], 1       ; Increment the Y position of the reward
    call movP                   ; Call movP subroutine to move the player
R2:
    loop F                      ; Loop back to F subroutine based on CX
    call UndrawObs              ; Call UndrawObs subroutine to clear the obstacle
    mov word [obsx], 0          ; Reset the X position of the obstacle
    mov word [obsy], 1          ; Reset the Y position of the obstacle
    mov cx, 1780                ; Set CX to the number of iterations for the game loop
    mov di, 0                   ; Reset DI to the starting offset in video memory


; M subroutine: Removes the last bit of rewards if movement has been stopped
M:
    cmp word [es:di], 0x0201    ; Compare the content at the current position with the reward
    jne K                        ; If not equal, jump to K (skip processing)
        call delay                ; Call delay subroutine
        call delay                ; Call delay subroutine again
        call delay                ; Call delay subroutine again
        mov word [es:di], 0x0720 ; Move the reward to an empty space
        jmp K
K:
    add di, 2                    ; Move to the next position in video memory
    add word [rewardy], 1        ; Increment the Y position of the reward
    loop M                       ; Loop back to M subroutine based on CX
    jmp kl                       ; Jump to kl (end of the subroutine)

kl:
; R5 subroutine: Generates explosives at random positions on the screen
R5:
    ; Generate explosive at random
    call genE
    cmp word [random], 38       ; Compare the generated random number with 38
    je R5                       ; If equal, jump to R5 (retry)
    cmp word [random], 39       ; Compare the generated random number with 39
    je R5                       ; If equal, jump to R5 (retry)
    mov ax, word [random]       ; Move the random number to AX
    mov word [obsx], ax         ; Set the X position of the obstacle to the random number
    push word [obstacle]
    call drawObs                ; Draw the obstacle at the new position
    popad
    ret


section .text
    global _start
; start subroutine: Initializes and starts the game loop
start:
    ; Display names and game title
    call cls                   ; Clear the screen
    mov di, 160                ; Set DI to the starting offset in video memory
    mov si, names              ; Set SI to the names string
    call drawString            ; Call drawString subroutine to display the names
    ; Wait for a key press to start the game
    mov ah, 0                  ; Set AH to 0 for waiting for a key press
    int 16h                    ; Call interrupt 16h for keyboard input

    ; Game loop
    mov word [score], 0        ; Initialize score for player 1
    mov word [score1], 0       ; Initialize score for player 2
    
    call cls                   ; Clear the screen
    call clear_board_screen    ; Clear the board screen
    call Board                 ; Draw the game board
    mov cx, 5                  ; Set CX to 5 (number of iterations for drawing initial objects)
    ; Drawing the car and explosive to start
    push word [Car]
    call drawCar
    push word [Car1]
    call drawCar1


; R1 subroutine: Generates explosives at random positions on the screen
R1:
    call genE                ; Call genE subroutine to generate a random number
    cmp word [random], 38    ; Compare the generated random number with 38
    je R1                    ; If equal, jump to R1 (retry)
    cmp word [random], 39    ; Compare the generated random number with 39
    je R1                    ; If equal, jump to R1 (retry)
    mov ax, word [random]    ; Move the random number to AX
    mov word [obsx], ax      ; Set the X position of the obstacle to the random number
    push word [obstacle]
    call drawObs             ; Draw the obstacle at the new position

mrT:
    mov si, cx               ; Move the value of CX to SI

; ok subroutine: Draws rewards multiple times, clears interrupts before writing characters
ok:
    cli                      ; Disable interrupts to avoid problems with successive interrupt calls
    call drawE               ; Call drawE subroutine to draw rewards
    sti                      ; Enable interrupts after writing characters
    sub si, 1                ; Subtract 1 from SI
    jnz ok                   ; If SI is not zero, jump to ok (continue drawing rewards)
    call movE                ; Call movE subroutine to move the rewards
    mov word [rewardy], 1    ; Reset the Y position of the rewards to 1
    call drawScores          ; Call drawScores subroutine to update scores
    loop mrT                 ; Loop back to mrT subroutine based on CX
    call UndrawObs           ; Call UndrawObs subroutine to remove the last obstacle
    call Winning             ; Call Winning subroutine
    mov ax, 0x4c00           ; Set exit code for DOS
    int 21h                   ; Call DOS interrupt 21h to exit

; movP subroutine: Moves the player's car based on position checks
movP:
    pushad             ; Push all general-purpose registers to the stack
    mov dx, 0          ; Initialize DX to 0
next:
    cmp word [carx], 22  ; Compare the X position of the car with 22
    jne A               ; If not equal, jump to A
    jmp C               ; Jump to C (car is at the top border)
A:
    mov word [checker], 0  ; Set the first checker to 0
C:
    cmp word [carx], 37  ; Compare the X position of the car with 37
    jne B               ; If not equal, jump to B
    jmp D               ; Jump to D (car is at the bottom border)
B:
    mov word [checker+2], 0  ; Set the third checker to 0

; D subroutine: Checks keyboard input to move the player's car
D:
    mov ah, 1          ; Set AH to 1 for checking if a key is pressed
    int 16h             ; Call interrupt 16h for keyboard input
    jz exit1           ; If no key is pressed, jump to exit1 (continue with the rest of the code)
    mov ah, 0          ; Set AH to 0 for reading the key pressed
    int 16h             ; Call interrupt 16h to read the key pressed
    cmp al, 'a'        ; Compare the key pressed with 'a'
    je check2          ; If equal, jump to check2 (move the car left)
    cmp al, 'd'        ; Compare the key pressed with 'd'
    je check10         ; If equal, jump to check10 (move the car right)
    cmp al, 'j'        ; Compare the key pressed with 'j' (for car1)
    je check3          ; If equal, jump to check3 (move car1 left)
    cmp al, 'l'        ; Compare the key pressed with 'l' (for car1)
    je check4          ; If equal, jump to check4 (move car1 right)
    cmp al, ' '        ; Compare the key pressed with ' ' (space)
    je exit1           ; If equal, jump to exit1 (exit)
    jmp next           ; Jump to next (continue checking for keys)
exit1:
    jmp exit           ; Jump to exit (exit the subroutine)

;BOrder check
; check10 subroutine: Checks if the player's car can move right
check10:
   cmp word [checker+2], 0  ; Compare the third checker with 0
   je L                      ; If equal, jump to L (move the car right)
   jmp next                  ; Jump to next (continue checking)

; check2 subroutine: Checks if the player's car can move left
check2:
   cmp word [checker], 0    ; Compare the first checker with 0
   je S                      ; If equal, jump to S (move the car left)
   jmp next                  ; Jump to next (continue checking)

; check3 subroutine: Checks if car1 can move left
check3:
    cmp word [checker+4], 0  ; Compare the fifth checker with 0
    je llo                   ; If equal, jump to llo (move car1 left)
    jmp next                 ; Jump to next (continue checking)

; check4 subroutine: Checks if car1 can move right
check4:
    cmp word [checker+8], 0  ; Compare the ninth checker with 0
    je llk                   ; If equal, jump to llk (move car1 right)
    jmp next                 ; Jump to next (continue checking)

;left
; S subroutine: Move the player's car left
S:
   call UndrawCar            ; Call UndrawCar to remove the previous position
   sub word [carx], 1        ; Subtract 1 from the car's X position
   push word [Car]
   call drawCar              ; Draw the car at the new position
   cmp word [carx], 22       ; Check if the car has reached the left border
   je O                      ; If yes, jump to O (update checker)
   jmp next                  ; Jump to next (continue checking)

; L subroutine: Move the player's car right
L:
   call UndrawCar            ; Call UndrawCar to remove the previous position
   add word [carx], 1        ; Add 1 to the car's X position
   push word [Car]
   call drawCar              ; Draw the car at the new position
   cmp word [carx], 37       ; Check if the car has reached the right border
   je N                      ; If yes, jump to N (update checker)
   jmp next                  ; Jump to next (continue checking)

; llo subroutine: Move car1 left (player 2)
llo:
   call UndrawCar1           ; Call UndrawCar1 to remove the previous position of car1
   sub word [car1x], 1       ; Subtract 1 from car1's X position
   push word [Car1]
   call drawCar1             ; Draw car1 at the new position
   cmp word [car1x], 41      ; Check if car1 has reached the left border
   je HJ                     ; If yes, jump to HJ (update checker)
   jmp next                  ; Jump to next (continue checking)

; llk subroutine: Move car1 right (player 2)
llk:
   call UndrawCar1           ; Call UndrawCar1 to remove the previous position of car1
   add word [car1x], 1       ; Add 1 to car1's X position
   push word [Car1]
   call drawCar1             ; Draw car1 at the new position
   cmp word [car1x], 56      ; Check if car1 has reached the right border
   je JH                     ; If yes, jump to JH (update checker)
   jmp next                  ; Jump to next (continue checking)

N: ; Checked the checker if the border is reached
   mov word [checker+2], 1   ; Update the second checker to indicate border reached
   jmp next                  ; Jump to next

O: ; Checked the checker if the border is reached
   mov word [checker], 1     ; Update the first checker to indicate border reached
   jmp next                  ; Jump to next

HJ: ; Checked the checker if the border is reached
   mov word [checker+4], 1   ; Update the fifth checker to indicate border reached
   jmp next                  ; Jump to next

JH: ; Checked the checker if the border is reached
   mov word [checker+8], 1   ; Update the ninth checker to indicate border reached
   jmp next                  ; Jump to next

exit:
    popad
    ret

Winning:
    ; Winning subroutine: Compare scores to determine the winner
    pushad                    ; Save registers
    mov ax, word[score1]      ; Load the score of player 2 (car1) into AX
    cmp word[score], ax       ; Compare the score of player 1 (car) with player 2
    ja S1                     ; Jump to S1 if player 1 has a higher score
    cmp word[score], ax       ; Compare the score of player 1 (car) with player 2
    je S3                     ; Jump to S3 if the scores are equal (tie)
    jmp S2                    ; Jump to S2 if player 2 has a higher score
; S1 subroutine: Player 1 (car) has a higher score
S1:
    mov dh, 11           ; Set the row (Y position)
    mov dl, 34           ; Set the column (X position)
    mov ah, 0x02         ; Set the cursor position
    int 0x10
    mov ah, 0x0e         ; Set the printing attributes
    mov bx, 0x0007       ; Set the color attribute
    mov si, player1_score_str1
    .next_char2:
        lodsb             ; Load the next character from the string
        cmp al, 0         ; Check if it's the end of the string
        jz done78         ; If yes, jump to done78
        mov cx, 1         ; Set the character count to 1
        int 0x10           ; Print the character
        jmp .next_char2   ; Jump to the next character

; S2 subroutine: Player 2 (car1) has a higher score
S2:
    mov dh, 11           ; Set the row (Y position)
    mov dl, 34           ; Set the column (X position)
    mov ah, 0x02         ; Set the cursor position
    int 0x10
    mov ah, 0x0e         ; Set the printing attributes
    mov bx, 0x0007       ; Set the color attribute
    mov si, player2_score_str1
    .next_char1:
        lodsb             ; Load the next character from the string
        cmp al, 0         ; Check if it's the end of the string
        jz done78         ; If yes, jump to done78
        mov cx, 1         ; Set the character count to 1
        int 0x10           ; Print the character
        jmp .next_char1   ; Jump to the next character
; S3 subroutine: Both players have the same score
S3:
    mov dh, 11           ; Set the row (Y position)
    mov dl, 34           ; Set the column (X position)
    mov ah, 0x02         ; Set the cursor position
    int 0x10
    mov ah, 0x0e         ; Set the printing attributes
    mov bx, 0x0007       ; Set the color attribute
    mov si, str12
    .next_char:
        lodsb             ; Load the next character from the string
        cmp al, 0         ; Check if it's the end of the string
        jz done78         ; If yes, jump to done78
        mov cx, 1         ; Set the character count to 1
        int 0x10           ; Print the character
        jmp .next_char    ; Jump to the next character

done78:
    popad        ; Restore all general-purpose registers
    ret          ; Return from subroutine

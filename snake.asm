model tiny
.code
org 100h
locals @@

_start:
	call parse_args
	call save_state
	call set_params
	call main
	call load_prev_state
	call end_prog
	
save_state proc
	push ax
	push di
	push es
	
	mov di, 0040h
	mov es, di
	
	mov di, 0062h
	mov ah, byte ptr es:[di]
	mov byte ptr [cur_page], ah
	
	mov di, 0049h
	mov ah, byte ptr es:[di]
	mov byte ptr [cur_mode], ah
	
	pop es
	pop di
	pop ax
	ret
save_state endp

set_params proc
	push ax
	push bx
	push cx
	push di
	push es
	
	mov ah, 00h
	mov al, work_mode
	int 10h
	
	mov ah, 05h
	mov al, work_page
	int 10h
	
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret
set_params endp

main proc
	push ax

	call add_int_handlers
	call init_game
	
	@@game_loop:
		mov al, [current_game_state]
		cmp al, 0
		jnz @@end_loop
		
		mov al, [handle_tick]
		cmp al, 1
		jz @@tick
		hlt
		jmp @@game_loop
		
		@@tick:
		xor al, al
		mov [handle_tick], al
		
		call handle_all_keys
		hlt
		
		jmp @@game_loop
		
@@end_loop:
	
	call revert_int_handlers
	
	pop ax
	ret
main endp

load_prev_state proc
	push ax
	push bx
	
	mov ah, 00h
	mov al, [cur_mode]
	int 10h
	
	mov ah, 05h
	mov al, [cur_page]
	int 10h
	
	pop bx
	pop ax
	ret
load_prev_state endp

end_prog proc
	mov ax, 4c00h
	int 21h
end_prog endp

;====================END_MAIN_CODE==========================
;=====================MAIN_LOGIC============================

init_game proc
	push si
	
	mov dx, 0000h
	mov cx, game_field_height
	mov al, cdWallVoltage
@@voltage:
	call set_field_cell
	inc dh
	loop @@voltage			;стена с напряжением
	
	xor dx, dx
	inc dl
	mov cx, game_field_width
	sub cx, 2
	mov al, cdWallTeleport
@@teleport1:
	call set_field_cell
	inc dl
	loop @@teleport1		;верхняя стена телепорта
	
	mov dh, game_field_height
	dec dh
	mov dl, 1
	mov cx, game_field_width
	sub cx, 2
	mov al, cdWallTeleport
@@teleport2:
	call set_field_cell
	inc dl
	loop @@teleport2
	
	mov dl, game_field_width
	dec dl
	xor dh, dh
	mov cx, game_field_height
	mov al, cdWallRubber
@@rubber:
	call set_field_cell
	inc dh
	loop @@rubber
	
	mov dx, 0202h
	mov al, 9
	call set_field_cell
	
	mov dx, 0101h
	mov al, cdSnakeBody
	call set_field_cell
	
	mov dx, 0102h
	mov al, cdSnakeBody
	call set_field_cell
	
	mov dx, 0103h
	mov al, cdSnakeHead
	call set_field_cell
	
	call draw_game_field
	
	pop si
	ret
init_game endp

convert_to_field proc	;dl,dh - позиция на поле; ВЫХОД: di - указатель на эл. на поле
	push ax
	push dx
	
	mov di, offset game_field
	xor ax, ax
	mov al, dh
	mov dh, game_field_width
	mul dh
	add di, ax
	xor dh, dh
	add di, dx
	
	pop dx
	pop ax
	ret
convert_to_field endp

get_field_cell proc		;dl,dh - позиция на поле; ВЫХОД: al - элемент поля
	push di

	call convert_to_field
	mov al, [di]
	
	pop di
	ret
get_field_cell endp

set_field_cell proc		;;dl,dh - позиция на поле, al - элемент поля
	push di
	
	call convert_to_field
	mov [di], al
	
	pop di
	ret
set_field_cell endp

cdEmpty 	 	equ 0
cdWallVoltage	equ 1
cdWallRubber	equ 2
cdWallTeleport	equ 3
cdSnakeBody		equ 4
cdSnakeHead		equ 5

game_field_width equ 35
game_field_height equ 25
game_field db game_field_width * game_field_height dup(0)			  

;===================END_MAIN_LOGIC==========================
;====================GAME_ENTITIES==========================

get_handler_for_entity proc 	;al - код сущности; ВЫХОД: bx - обработчик графики, dx - обработчик пересечения
	push si
	
	mov si, offset entTable
	
@@loop:
	mov bx, [si]
	cmp bx, 0FFFFh
	jz @@set_handlers
	cmp bl, al
	jz @@set_handlers
	add si, 6
	jmp @@loop
	
@@set_handlers:
	mov bx, [si + 2]
	mov dx, [si + 4]
	
	pop si
	ret
get_handler_for_entity endp

entTable dw cdEmpty, offset draw_empty, 0
		 dw cdWallVoltage, offset draw_wall_voltage, 0
		 dw cdWallRubber, offset draw_wall_rubber, 0
		 dw cdWallTeleport, offset draw_wall_teleport, 0
		 dw cdSnakeBody, offset draw_snake_body, 0
		 dw cdSnakeHead, offset draw_snake_head, 0
		 dw	0FFFFh, 0, 0

;==================END_GAME_ENTITIES========================
;========================VIEW===============================

draw_game_field proc
	push ax
	push dx
	push si
	
	mov si, offset game_field
	xor dx, dx
@@loop:
	mov al, [si]
	call draw_handler
	
	inc si
	inc dl
	cmp dl, game_field_width
	jnz @@loop
	xor dl, dl
	inc dh
	cmp dh, game_field_height
	jnz @@loop
	
	pop si
	pop dx
	pop ax
	ret
draw_game_field endp

draw_handler proc		;dl,dh - позиция на карте, al - код рисуемого объекта
	push bx
	push dx
	push di
	push es
	
	mov bx, graph_buffer
	mov es, bx
	call convert_position
	
	call get_handler_for_entity
	cmp bx, 0
	jz @@default
	call bx
	jmp @@end
	
@@default:
	call draw_default

@@end:
	pop es
	pop di
	pop dx
	pop bx
	ret
draw_handler endp

convert_position proc	;dl,dh - позиция на карте; ВЫХОД: di - позиция в буффере
	push ax
	push dx
	
	xor ax, ax
	mov al, dh
	mov dh, draw_width
	mul dh
	shl ax, 2
	mov di, ax
	
	xor dh, dh
	shl dl, 1
	add di, dx
	
	pop dx
	pop ax
	ret
convert_position endp

draw_two_line proc	;ax - 8 пикселей в первой линии, bx - во второй, di - указывает под эти две строки
	stosw
	add di, 2000h - 2
	xchg ax, bx
	stosw
	sub di, 2000h - 78
	xchg ax, bx
	
	ret
draw_two_line endp

;======================END_VIEW=============================
;====================DRAW_HANDLERS==========================

draw_empty proc
	push ax
	push bx
	push cx
	push di
	push es

	mov cx, 4
@@loop:
	xor ax, ax
	xor bx, bx
	call draw_two_line
	loop @@loop
	
	pop es
	pop di
	pop cx
	pop bx
	pop ax
	ret
draw_empty endp

draw_default proc		;es:di - указатель на позицию;
	push ax
	push cx
	push di
	push es
	
	mov cx, 4
@@loop:
	mov ax, 0AAAAh
	mov bx, 05555h
	call draw_two_line
	loop @@loop
	
	pop es
	pop di
	pop cx
	pop ax
	ret
draw_default endp

draw_wall_voltage proc
	push ax
	push bx
	push cx
	
	mov ax, 7777h
	mov bx, ax
	mov cx, 2
@@loop1:
	call draw_two_line
	loop @@loop1
	
	mov ax, 0DDDDh
	mov bx, ax
	mov cx, 2
@@loop2:
	call draw_two_line
	loop @@loop2
	
	pop cx
	pop bx
	pop ax
	ret
draw_wall_voltage endp

draw_wall_rubber proc
	push ax
	push bx
	push cx
	
	mov ax, 0AAAAh
	mov bx, ax
	mov cx, 4
@@loop:
	call draw_two_line
	loop @@loop
	
	pop cx
	pop bx
	pop ax
	ret
draw_wall_rubber endp

draw_wall_teleport proc
	push ax
	push bx
	push cx
	
	mov ax, 0CCCCh
	mov bx, ax
	mov cx, 4
@@loop:
	call draw_two_line
	loop @@loop
	
	pop cx
	pop bx
	pop ax
	ret
draw_wall_teleport endp

draw_snake_body proc
	push ax
	push bx
	push cx
	
	mov ax, 0A00Ah
	mov bx, 0A82Ah
	call draw_two_line
	
	mov cx, 2
	mov ax, 0AAAAh
	mov bx, 0AAAAh
@@loop:
	call draw_two_line
	loop @@loop
	
	mov ax, 0A82Ah
	mov bx, 0A00Ah
	call draw_two_line
	
	pop cx
	pop bx
	pop ax
	ret
draw_snake_body endp

draw_snake_head proc
	push ax
	push bx
	
	mov ax, 0A00Ah
	mov bx, 0A82Ah
	call draw_two_line

	mov ax, 0AAAAh
	mov bx, 0BAAEh
	call draw_two_line
	
	mov ax, 0BAAEh
	mov bx, 0AAAAh
	call draw_two_line
	
	mov ax, 0A82Ah
	mov bx, 0A00Ah
	call draw_two_line
	
	pop bx
	pop ax
	ret
draw_snake_head endp

;==================END_DEAW_HANDLERS========================
;======================KEYBOARD=============================

handle_all_keys proc
	push ax
	push bx

@@loop:
	call pop_from_buffer
	cmp al, 0
	jz @@end_handle
	call get_handler_key
	cmp bx, 0
	jz @@loop
	call bx
	jmp @@loop
	
@@end_handle:
	pop bx
	pop ax
	ret
handle_all_keys endp

;====================END_KEYBOARD===========================
;==================KEYBOARD_HANDLERS========================

get_handler_key proc 	;al - код кнопки; ВЫХОД: bx - обработчик кнопки
	push si
	
	mov si, offset keyTable
@@loop:
	mov bx, [si]
	cmp bx, 0FFFFh
	jz @@set_handler
	cmp bl, al
	jz @@set_handler
	add si, 4
	jmp @@loop
	
@@set_handler:
	mov bx, [si + 2]
	
	pop si
	ret
get_handler_key endp

keyTable 	dw 1, offset end_key_handler
			dw 0FFFFh, 0
			
end_key_handler proc
	push ax
	
	mov al, 1
	mov [current_game_state], al
	
	pop ax
	ret
end_key_handler endp

;================END_KEYBOARD_HANDLERS======================
;=======================BUFFER==============================

push_to_buffer proc		;al - добавляемый символ
	push ax
	push bx
	push di

	mov ah, cs:[tail]
	mov di, offset cs:buffer
	push ax
	mov al, ah
	xor ah, ah
	add di, ax
	pop ax
	mov cs:[di], al
	
	call inc_head_or_tail
	mov cs:[tail], ah
	
	mov bl, cs:[head]
	mov bh, cs:[tail]
	cmp bl, bh
	jnz @@next
	
	mov ah, bl
	call inc_head_or_tail
	mov cs:[head], ah
	
@@next:
	pop di
	pop bx
	pop ax
	ret
push_to_buffer endp

pop_from_buffer proc	;ВЫХОД: al - извлечённый символ
	push bx
	push di

	mov bl, cs:[head]
	mov bh, cs:[tail]
	cmp bl, bh
	jz @@pop_empty_val
	
	mov ah, cs:[head]
	mov di, offset cs:buffer
	push ax
	mov al, ah
	xor ah, ah
	add di, ax
	pop ax
	mov al, cs:[di]
	call inc_head_or_tail
	mov cs:[head], ah
	jmp @@end_proc
	
@@pop_empty_val:
	xor al, al
@@end_proc:
	pop di
	pop bx
	ret
pop_from_buffer endp

inc_head_or_tail proc	;al - значение хвоста или головы
	inc ah
	cmp ah, buffer_size
	jnz @@end_inc
	xor ah, ah
@@end_inc:
	ret
inc_head_or_tail endp

buffer_size = 8
head db 0
tail db 0
buffer db buffer_size dup(0)

;=====================END BUFFER============================
;======================HANDLERS=============================

add_int_handlers proc
	call add_int08
	call add_int09
	ret
add_int_handlers endp

revert_int_handlers proc
	call revert_int08
	call revert_int09
	ret
revert_int_handlers endp

int09 proc
	push ax
	push di
	push es
	
	in	al,	60h
	call push_to_buffer
	pop es
	pop di
	in al, 61h
	mov ah, al
	or al, 80h
	out 61h, al
	nop
	nop
	nop
	xchg ah, al
	out 61h, al
	mov al, 20h
	out 20h, al
	pop ax 	
	iret
int09 endp

add_int09 proc
	push ax
	push di
	push si
	push es

	mov si, 4*9
	mov di, offset old_int_9
	xor ax, ax
	mov ds, ax
	movsw
	movsw
	
	push ds
	pop es
	push cs
	pop ds
	mov ax, offset int09
	mov di, 4*9
	cli
	stosw
	mov ax, cs
	stosw
	sti

	; восстановим все регистры в наш сегмент
	push cs
	pop ds
	
	pop es
	pop si
	pop di
	pop ax
	ret
add_int09 endp

revert_int09 proc
	push ax
	push di
	push si
	push es
	
	mov di, 4*9
	mov si, offset old_int_9
	xor ax, ax
	mov es, ax
	cli
	movsw
	movsw
	sti
	
	pop es
	pop si
	pop di
	pop ax
	ret
revert_int09 endp

int08 proc 
	push ax
	mov al, cs:[counter]
	cmp al, skip_ticks_to_handle_int08
	jz @@handle
	inc cs:[counter]
	jmp @@next
@@handle:
	xor al, al
	mov cs:[counter], al
	inc al
	mov cs:[handle_tick], al
@@next:
	pop ax
	db	0eah
old_int_8 dw	0, 0
counter db	0
int08 endp

add_int08 proc
	push ax
	push di
	push si
	push es

	mov si, 4*8
	mov di, offset old_int_8
	xor ax, ax
	mov ds, ax
	movsw
	movsw
	
	push ds
	pop es
	push cs
	pop ds
	mov ax, offset int08
	mov di, 4*8
	cli
	stosw
	mov ax, cs
	stosw
	sti

	; восстановим все регистры в наш сегмент
	push cs
	pop ds
	
	pop es
	pop si
	pop di
	pop ax
	ret
add_int08 endp

revert_int08 proc
	push ax
	push di
	push si
	push es
	
	mov di, 4*8
	mov si, offset old_int_8
	xor ax, ax
	mov es, ax
	cli
	movsw
	movsw
	sti
	
	pop es
	pop si
	pop di
	pop ax
	ret
revert_int08 endp

;====================END_HANDLERS===========================
;========================TOOLS==============================

parse_str_to_int proc	;si - указатель на начало строки, bl - основание СС; ВЫХОД - al - прочитанное число
	push bx
	
	xor al, al
	
	mov bh, [si]
	cmp bh, '0'
	jl @@empty_int
	cmp bh, '9'
	jg @@empty_int 
	jmp @@loop
	
	@@empty_int:
	stc
	pop bx
	ret
	
	@@loop:
		mov bh, [si]
		cmp bh, '0'
		jl @@end_loop
		cmp bh, '9'
		jg @@end_loop
		
		sub bh, '0'
		mul bl
		add al, bh
		inc si
		jmp @@loop
	
	@@end_loop:
	clc
	
	pop bx
	ret
parse_str_to_int endp

parse_int_to_str proc	;ax - число, bl - СС, di - место, куда будет записано число
	push ax
	push di
	
	@@skip:
		cmp byte ptr [di], ' '
		jnz @@end_skip
		inc di
		jmp @@skip
	@@end_skip:
	dec di
	
	@@loop:
		cmp al, 0
		jz @@end
	
		xor ah, ah
		div bl
		add ah, '0'
		mov [di], ah

		dec di
		jmp @@loop
		
	@@end:
	pop di
	pop ax
	ret
parse_int_to_str endp

;=========================END TOOLS======================
;===========================PARSE ARGS=============================

end_with_err proc	;al - код ошибки
	push ax
	xor ah, ah
	mov di, offset error_code
	mov bl, 16
	call parse_int_to_str
	pop ax
	
	mov dx, offset error_msg
	mov ah, 09h
	int 21h

	mov ah, 4ch
	int 21h
end_with_err endp

parse_args proc
	push ax
	push bx
	push si

	mov si, offset ds:81h
	
	@@args:
		mov bl, [si]
		cmp bl, 0dh
		jz @@end_args

		cmp bl, ' '
		jnz @@end_space
		inc si
		jmp @@args

		@@end_space:
		cmp bl, '-'
		jz @@read_arg
		
		mov al, 01h
		call end_with_err
		
		@@read_arg:
			inc si
			mov bl, [si]

			cmp bl, 0dh
			jz end_with_err
			
			jmp @@parse_letter
			
			@@help:
			call help
			jmp @@args
			
			@@parse_letter:
			cmp bl, 'h'
			jz @@help
			cmp bl, 'H'
			jz @@help
			
			mov al, 02h
			call end_with_err
	
@@end_args:
	pop si
	pop bx
	pop ax
	ret
parse_args endp

help proc		;si - байт перед пробелом и параметром
	push ax
	push bx
	
	mov ah, 09h
	mov dx, offset help_msg
	int 21h
	
	call end_prog
	
	pop bx
	pop ax
	ret
help endp

;АРГУМЕНТЫ КОМАНДНОЙ СТРОКИ===============================
;Последний код ошибки: 05h

;ГЛОМАБЛЬНЫЕ ПАРАМЕТРЫ===================================-
background_color equ 0
graph_buffer equ 0B800h
work_mode equ 04h
work_page equ 0
draw_width equ 80
skip_ticks_to_handle_int08 equ 4

cur_mode db 0
cur_page db 0
handle_tick db 0

current_game_state db 0

old_int_9 	dw 0, 0

;СООБЩЕНИЯ================================================

help_msg db "HELP$"
error_msg db "Incorrect program state. Code:"
error_code db "  h"
endl db 0dh, 0ah, 24h

end _start

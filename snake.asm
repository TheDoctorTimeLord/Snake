model tiny

handler_key_direction macro direct, oposit
	call get_snake_dir
	cmp al, oposit
	jz @@end
	mov al, direct
	mov [changed_direction], al
@@end:
endm

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
		call check_end_rule
		cmp al, 0
		jg @@end_loop
		
		call handle_all_keys
		
		mov al, [handle_tick]
		cmp al, 0
		jnz @@tick
		hlt
		jmp @@game_loop
		
	@@tick:
		call update_game_on_tick
		
		cmp al, byte ptr[skip_update]
		jl @@game_loop
		
		xor al, al
		mov [handle_tick], al
		
		mov al, [on_pause]
		cmp al, 1
		jz @@after_update
		call update_snake
	@@after_update:
		
		call draw_window
		
		jmp @@game_loop
		
@@end_loop:
	cmp al, state_hard_leave
	jz @@end
	
	;показ результатов
	
	@@loop:
		call handle_all_keys
		mov al, [current_game_state]
		cmp al, state_hard_leave
	jnz @@loop
		
	
@@end:
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
	push ax
	push cx
	push dx
	push si
	
	call srandsystime
	
	mov dx, 0101h
	call add_coordinate_to_buffer
	mov dx, 0202h
	call add_coordinate_to_buffer
	mov dx, 0303h
	call add_coordinate_to_buffer
	mov ax, 0
	call remove_coordinate_from_buffer
	mov ax, 1
	call remove_coordinate_from_buffer
	mov ax, 0
	call remove_coordinate_from_buffer
	
	mov al, start_skip_upd
	mov [skip_update], al
	
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
	loop @@teleport2		;нижняя стена телепорта
	
	mov dl, game_field_width
	dec dl
	xor dh, dh
	mov cx, game_field_height
	mov al, cdWallRubber
@@rubber:
	call set_field_cell
	inc dh
	loop @@rubber			;стена с отскоком
	
	mov dx, 0101h
	mov al, 1
	call add_snake_head
	call add_snake_seg
	mov al, 2
	call set_head_dir
	call add_snake_seg
	call add_snake_seg
	call move_snake			;начальное положение змейки
	
	xor al, al
	call set_snake_on_field
	
	call get_snake_dir
	mov [changed_direction], al
	
	xor ch, ch
	mov cl, [started_add_seg]
	mov al, cdAddSeg
	call spawn_ardtefacts
	
	mov cl, [started_damage]
	mov al, cdDamage
	call spawn_ardtefacts
	
	mov cl, [started_dead]
	mov al, cdDead
	call spawn_ardtefacts	;начальный спавн артефактов
	
	call draw_game_field
	
	pop si
	pop dx
	pop cx
	pop ax
	ret
init_game endp

spawn_ardtefacts proc	;al - код артефакта, cx - количество артефактов
	push cx
	
	cmp cx, 0
	jz @@end
	
	@@loop:
		call set_random_game_sell
	loop @@loop
	
@@end:
	pop cx
	ret
spawn_ardtefacts endp

check_end_rule proc		;al - текущее состояние игры
	push bx
	
	mov al, [current_game_state]
	cmp al, state_continue
	jg @@end
	
	mov bl, [current_len_snake]
	cmp bl, 0
	jnz @@next
	
	mov al, state_lose
	jmp @@end
	
@@next:
	cmp bl, max_len_snake
	jnz @@end
	
	mov al, state_win
	
@@end:
	pop bx
	ret
check_end_rule endp

update_game_on_tick proc
	push ax
	
	xor al, al
	mov [is_updated_on_cur_tick], al
	
	pop ax
	ret
update_game_on_tick endp

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

set_field_cell proc		;dl,dh - позиция на поле, al - элемент поля
	push di
	
	call convert_to_field
	mov [di], al
	
	pop di
	ret
set_field_cell endp

game_field_width equ 32
game_field_height equ 25
game_field db game_field_width * game_field_height dup(0)			  

;====================END_MAIN_LOGIC=========================
;==================RANDOM_FIELD_CELL========================

set_random_game_sell proc		;al - код добавляемого объекта
	push ax
	push bx
	push dx
	
	mov bl, al
	
	mov al, [is_updated_on_cur_tick]
	cmp al, 1
	jz @@next
	call fill_buffer_with_empty_cells
@@next:
	
	mov ax, [len_game_sells]
	cmp ax, 0
	jz @@end
	
	call randint
	call get_coordinate_from_buffer
	call remove_coordinate_from_buffer
	mov al, bl
	call set_field_cell
	
@@end:
	pop dx
	pop bx
	pop ax
	ret
set_random_game_sell endp

fill_buffer_with_empty_cells proc
	push ax
	push dx
	
	call clear_sells_buffer
	xor dx, dx
	
	@@loop:
		call get_field_cell
		cmp al, cdEmpty
		jnz @@continue
		
		call add_coordinate_to_buffer
	
	@@continue:
		inc dl
		cmp dl, game_field_width
		jnz @@loop
		xor dl, dl
		inc dh
		cmp dh, game_field_height
	jnz @@loop
	
	mov al, 1
	mov [is_updated_on_cur_tick], al
	
	pop dx
	pop ax
	ret
fill_buffer_with_empty_cells endp

add_coordinate_to_buffer proc		;dl,dh - координаты клетки
	push ax
	push si
	
	mov ax, [len_game_sells]
	call get_pointer_on_cell
	mov [si], dl
	mov [si + 1], dh
	inc [len_game_sells]
	
	pop si
	pop ax
	ret
add_coordinate_to_buffer endp

remove_coordinate_from_buffer proc	;ax - номер в массиве
	push ax
	push si
	
	call get_pointer_on_cell
	
	xor al, al
	mov [si], al
	mov [si + 1], al
	add si, size_struct_sell
	
	@@loop:
		mov al, [si]
		mov ah, [si + 1]
		cmp al, 0
		jnz @@next
		cmp ah, 0
		jnz @@next
		jmp @@end
	@@next:
		mov [si - 2], al
		mov [si - 1], ah
		xor al, al
		mov [si], al
		mov [si + 1], al
		add si, size_struct_sell
	jmp @@loop
@@end:
	dec [len_game_sells]
	pop si
	pop ax
	ret
remove_coordinate_from_buffer endp

clear_sells_buffer proc
	push ax
	
	xor ax, ax
	mov [len_game_sells], ax
	
	pop ax
	ret
clear_sells_buffer endp

get_coordinate_from_buffer proc		;ax - номер в массиве; ВЫХОД: dl,dh - координаты
	push si
	
	call get_pointer_on_cell
	mov dl, [si]
	mov dh, [si + 1]
	
	pop si
	ret
get_coordinate_from_buffer endp

get_pointer_on_cell proc	;ax - номер в массиве; ВЫХОД: si - указатель на координаты
	push ax
	
	shl ax, 1
	mov si, offset sells_buffer
	add si, ax

	pop ax
	ret
get_pointer_on_cell endp

size_struct_sell equ 2
is_updated_on_cur_tick db 0
len_game_sells dw 0
sells_buffer db size_struct_sell * game_field_width * game_field_height dup(0)
empty_sells db 0, 0

;================END_RANDOM_FIELD_CELL======================
;========================SNAKE==============================

update_snake proc
	push ax
	
	mov al, [changed_direction]
	call set_head_dir
	call move_snake_with_handle
	
	pop ax
	ret
update_snake endp

move_snake_with_handle proc
	push ax
	push bx
	push dx
	
	call get_cell_front_head
	call get_field_cell
	call get_handler_for_entity
	
	xor al, al
	cmp dx, 0
	jz @@next
	call dx
@@next:
	mov ah, 1
	call set_snake_on_field
	cmp al, 1
	jz @@next1
	call move_snake
@@next1:
	xor ah, ah
	call set_snake_on_field
	
	pop dx
	pop bx
	pop ax
	ret
move_snake_with_handle endp

move_snake proc
	push ax
	push cx
	push dx
	push si
	
	mov ah, [current_len_snake]
	cmp ah, 0
	jz @@end
	
	dec ah
	
	mov al, ah
	call get_pointer_on_seg

	@@loop:
		cmp si, offset snake_array
		jz @@move_head
		mov dl, [si - 3]
		mov dh, [si - 2]
		mov al, [si - 1]
		mov [si], dl
		mov [si + 1], dh
		mov [si + 2], al
		sub si, size_snake_elem
	jmp @@loop
	
@@move_head:
	mov dl, [si]
	mov dh, [si + 1]
	mov al, [si + 2]
	call offset_coordinate
	mov [si], dl
	mov [si + 1], dh
	mov [si + 2], al

@@end:
	pop si
	pop dx
	pop cx
	pop ax
	ret
move_snake endp

add_snake_seg proc
	push ax
	push dx
	push si
	
	mov ah, [current_len_snake]
	dec ah
	mov al, ah
	call get_pointer_on_seg
	mov dl, [si]
	mov dh, [si + 1]
	mov al, [si + 2]
	call move_snake
	add si, size_snake_elem
	mov [si], dl
	mov [si + 1], dh
	mov [si + 2], al
	inc [current_len_snake]
	
	pop si
	pop dx
	pop ax
	ret
add_snake_seg endp

add_snake_head proc		;dl,dh - координаты головы, al - направление движения
	push di
	
	mov di, offset snake_array
	mov [di], dl
	mov [di + 1], dh
	mov [di + 2], al
	inc [current_len_snake]
	
	pop di
	ret
add_snake_head endp

remove_last_seg proc
	push ax
	push si
	
	mov al, [current_len_snake]
	cmp al, 0
	jz @@end
	
	call get_pointer_on_last_seg
	xor al, al
	mov [si], al
	mov [si + 1], al
	mov [si + 2], al
	dec [current_len_snake]
	
@@end:
	pop si
	pop ax
	ret
remove_last_seg endp

remove_from_seg proc 	;cl - с какого сегмента удалять элементы
	push cx
	
	mov ch, [current_len_snake]
	cmp cl, ch
	jz @@end
	
	inc cl
	
	@@loop:
		mov ch, [current_len_snake]
		cmp cl, ch
		jz @@end
		call remove_last_seg
	jmp @@loop
	
@@end:
	pop cx
	ret
remove_from_seg endp

set_head_dir proc		;al - направление движения
	push ax
	push di
	
	mov ah, [current_len_snake]
	cmp ah, 0
	jz @@end
	
	mov di, offset snake_array
	mov [di + 2], al
	
@@end:
	pop di
	pop ax
	ret
set_head_dir endp

set_head_coordinate proc	;dl,dh - новые координаты
	push di
	
	mov di, offset snake_array
	mov [di], dl
	mov [di + 1], dh
	
	pop di
	ret
set_head_coordinate endp

get_cell_front_head proc	;ВЫХОД: dl,dh - координаты клетки, куда смотрит змейка
	push ax
	push si
	
	mov si, offset snake_array
	mov dl, [si]
	mov dh, [si + 1]
	mov al, [si + 2]
	call offset_coordinate
	
	pop si
	pop ax
	ret
get_cell_front_head endp

get_pointer_on_seg proc		;al - номер сегмента; ВЫХОД: si - указатель на сегмент
	push ax
	push bx
	
	mov si, offset snake_array
	xor ah, ah
	mov bl, size_snake_elem
	mul bl
	add si, ax
	
	pop bx
	pop ax
	ret
get_pointer_on_seg endp

get_pointer_on_last_seg proc	;ВЫХОД: si - указатель на последний сегмент
	push ax
	
	mov al, [current_len_snake]
	cmp al, 0
	jz @@next
	dec al
@@next:
	call get_pointer_on_seg
	
	pop ax
	ret
get_pointer_on_last_seg endp

set_snake_on_field proc		;ah (0 - рисовать змейку, 1 - затереть змейку)
	push ax
	push cx
	push dx
	push si
	
	mov al, [current_len_snake]
	cmp al, 0
	jz @@end
	
	call get_pointer_on_last_seg
	cmp ah, 0
	jz @@draw
	mov al, cdEmpty
	jmp @@after_draw
@@draw:
	mov al, cdSnakeBody
@@after_draw:
	xor ch, ch
	mov cl, [current_len_snake]
	
	@@loop:
		mov dl, [si]
		mov dh, [si + 1]
		cmp ah, 1
		jz @@next
		cmp si, offset snake_array
		jnz @@next
		mov al, cdSnakeHead
	@@next:
		call set_field_cell
		sub si, size_snake_elem
	loop @@loop

@@end:
	pop si
	pop dx
	pop cx
	pop ax
	ret
set_snake_on_field endp

get_snake_dir proc		;al - направление движения змейки
	push si
	
	mov si, offset snake_array
	mov al, [si + 2]
	
	pop si
	ret
get_snake_dir endp

get_num_snake_seg_by_coord proc		;dl,dh - координаты сегмента; ВЫХОД: cl - номер сегмента, ch - не сохраняется
	push bx
	push dx
	push si
	
	mov si, offset snake_array
	xor cx, cx
	mov ch, [current_len_snake]
	
	@@loop:
		cmp cl, ch
		jz @@end
		mov bl, [si]
		mov bh, [si + 1]
		cmp bl, dl
		jnz @@next
		cmp bh, dh
		jnz @@next
		jmp @@end
	@@next:
		add si, size_snake_elem
		inc cl
		jmp @@loop
	
@@end:
	pop si
	pop dx
	pop bx
	ret
get_num_snake_seg_by_coord endp


max_len_snake equ 30
size_snake_elem equ 3
changed_direction db 0
current_len_snake db 0
snake_array db max_len_snake * size_snake_elem dup(0)
;[0,1,x | 1,1,y | 2,1,dirrection]

;======================END_SNAKE============================
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

cdEmpty 	 	equ 0
cdWallVoltage	equ 1
cdWallRubber	equ 2
cdWallTeleport	equ 3
cdSnakeBody		equ 4
cdSnakeHead		equ 5
cdAddSeg		equ 6
cdDamage		equ 7
cdDead			equ 8

entTable dw cdEmpty, offset draw_empty, 0
		 dw cdWallVoltage, offset draw_wall_voltage, offset inter_wall_voltage
		 dw cdWallRubber, offset draw_wall_rubber, offset inter_wall_rubber
		 dw cdWallTeleport, offset draw_wall_teleport, offset inter_wall_teleport
		 dw cdSnakeBody, offset draw_snake_body, offset inter_snake_body
		 dw cdSnakeHead, offset draw_snake_head, 0
		 dw cdAddSeg, offset draw_add_seg, offset inter_add_seg
		 dw cdDead, offset draw_dead, offset inter_wall_voltage
		 dw cdDamage, offset draw_damage, offset inter_damage
		 dw	0FFFFh, 0, 0

;==================END_GAME_ENTITIES========================
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

draw_add_seg proc
	push ax
	push bx
	
	mov ax, 00002h
	mov bx, 05002h
	call draw_two_line
	
	mov ax, 00002h
	mov bx, 0A00Ah
	call draw_two_line
	
	mov ax, 0A82Eh
	mov bx, 0AABAh
	call draw_two_line
	
	mov ax, 0A82Ah
	mov bx, 0A00Ah
	call draw_two_line
	
	pop bx
	pop ax
	ret
draw_add_seg endp

draw_damage proc
	push ax
	push bx
	
	mov ax, 05005h
	mov bx, 04001h
	call draw_two_line
	
	mov ax, 04001h
	mov bx, 01004h
	call draw_two_line
	
	mov ax, 0A41Bh
	mov bx, 0A41Eh
	call draw_two_line
	
	mov ax, 0A96Ah
	mov bx, 05555h
	call draw_two_line
	
	pop bx
	pop ax
	ret
draw_damage endp

draw_dead proc
	push ax
	push bx
	
	mov ax, 0F00Fh
	mov bx, 0FC3Fh
	call draw_two_line
	
	mov ax, 0EBFAh
	mov bx, 0EBFAh
	call draw_two_line
	
	mov ax, 03FFFh
	mov bx, 0FC0Fh
	call draw_two_line
	
	mov ax, 0CC0Ch
	mov bx, 00000h
	call draw_two_line
	
	pop bx
	pop ax
	ret
draw_dead endp

;==================END_DEAW_HANDLERS========================
;================INTERSECTION_HANDLERS======================
;ВЫХОД: al - нужно ли перемещать (0 - нужно, 1 - нет)

inter_wall_voltage proc
	mov al, state_lose
	mov [current_game_state], al
	ret
inter_wall_voltage endp

inter_wall_rubber proc
	mov al, 0
	call set_head_dir
	call move_snake_with_handle
	mov al, 3
	mov [changed_direction], al
	mov al, 1
	ret
inter_wall_rubber endp

inter_wall_teleport proc
	push dx
	
	mov ah, 1
	call set_snake_on_field
	call move_snake
	call get_cell_front_head
	call get_snake_dir
	cmp al, 0
	jz @@up_coord
	mov dh, 1
	jmp @@next
@@up_coord:
	mov dh, game_field_height
	sub dh, 2
@@next:
	call set_head_coordinate
	mov al, 1
	
	pop dx
	ret
inter_wall_teleport endp

inter_snake_body proc
	mov al, [mode_intersection_with_yourself]
	cmp al, 0
	jz @@end
	cmp al, 2
	jz @@cut
	mov al, state_lose
	mov [current_game_state], al
	jmp @@end
@@cut:
	push ax
	push cx
	push dx
	mov ah, 1
	call set_snake_on_field
	call get_cell_front_head
	call get_num_snake_seg_by_coord
	call remove_from_seg
	pop dx
	pop cx
	pop ax
@@end:
	xor al, al
	ret
inter_snake_body endp

inter_add_seg proc
	call add_snake_seg
	mov al, cdAddSeg
	call set_random_game_sell
	mov al, 1
	ret
inter_add_seg endp

inter_damage proc
	push ax
	mov ah, 1
	call set_snake_on_field
	call remove_last_seg
	pop ax
	xor al, al
	ret
inter_damage endp

;==============END_INTERSECTION_HANDLERS====================
;========================VIEW===============================

draw_window proc
	
	call draw_game_field
	
	ret
draw_window endp

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
			dw 145, offset change_snake_dir_up
			dw 159, offset change_snake_dir_down
			dw 158, offset change_snake_dir_left
			dw 160, offset change_snake_dir_right
			dw 185, offset set_on_off_pause
			dw 179, offset reduce_game_speed
			dw 180, offset increase_game_speed
			dw 0FFFFh, 0
			
end_key_handler proc
	push ax
	
	mov al, state_hard_leave
	mov [current_game_state], al
	
	pop ax
	ret
end_key_handler endp

change_snake_dir_up proc
	push ax
	handler_key_direction 0, 2
	pop ax
	ret
change_snake_dir_up endp

change_snake_dir_down proc
	push ax
	handler_key_direction 2, 0
	pop ax
	ret
change_snake_dir_down endp

change_snake_dir_left proc
	push ax
	handler_key_direction 3, 1
	pop ax
	ret
change_snake_dir_left endp

change_snake_dir_right proc
	push ax
	handler_key_direction 1, 3
	pop ax
	ret
change_snake_dir_right endp

set_on_off_pause proc
	push ax
	
	mov al, [on_pause]
	xor al, 1
	mov [on_pause], al

	pop ax
	ret
set_on_off_pause endp

reduce_game_speed proc
	push ax
	
	mov al, [skip_update]
	cmp al, max_skip_upd
	jz @@end
	inc [skip_update]
@@end:
	pop ax
	ret
reduce_game_speed endp

increase_game_speed proc
	push ax
	
	mov al, [skip_update]
	cmp al, min_skip_upd
	jz @@end
	dec [skip_update]
@@end:
	pop ax
	ret
increase_game_speed endp

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
	inc cs:[handle_tick]
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
		jc @@empty_int
		jo @@empty_int
		add al, bh
		jc @@empty_int
		jo @@empty_int
		inc si
		jmp @@loop
	
	@@end_loop:
	clc
	
	pop bx
	ret
parse_str_to_int endp

parse_int_to_str proc	;ax - число, bl - СС, di - место, куда будет записано число, flag C = 1, если произошла ошибка
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

;dl,dh - координаты, al (0 - (0,-1); 1 - (1,0); 2 - (0,1); 3 - (-1,0))
;ВЫХОД: dl,dh сдвинутые в соответствии со сдвигом
offset_coordinate proc
	cmp al, 0
	jnz @@next
	dec dh
	jmp @@end
@@next:
	cmp al, 1
	jnz @@next1
	inc dl
	jmp @@end
@@next1:
	cmp al, 2
	jnz @@next2
	inc dh
	jmp @@end
@@next2:
	dec dl
@@end:
	ret
offset_coordinate endp

;=========================END_TOOLS========================
;==========================RANDOM==========================
 
srandsystime proc
    push ax
	push cx
	push dx
	
    xor ax, ax          ;получение системного времени CX:DX
    int 1ah
    mov [seed], dx
    
	pop dx
	pop cx
	pop ax
    ret
srandsystime endp
 
randint proc    ;ax - максимальная граница; ВЫХОД: ax - случайное число от 0 до ax
	push cx
	push dx
	push ax
	
    mov ax, 25173           ; LCG мультипликатор
    mul word ptr [seed]
    add ax, 13849
    mov [seed], ax
    ; AX = (мультипликатор * seed + инскримент) % 65536
   
	pop cx
	cmp cx, 0
	jnz @@next
	xor ax, ax
	jmp @@end
	
@@next:
	xor dx, dx
	div cx
	mov ax, dx
   
@@end:
	pop dx
	pop cx
	ret
randint endp

seed dw 11

;========================END_RANDOM========================
;========================PARSE_ARGS========================

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
		
		@@parse_err:
		mov al, 01h
		call end_with_err
		
		@@read_arg:
			inc si
			mov bl, [si]

			cmp bl, 0dh
			jz @@parse_err
			
			jmp @@parse_letter
			
			@@help:
			call help
			jmp @@args
			
			@@intersection:
			call intersection
			jmp @@args
			
			@@add_seg_art:
			mov ax, offset started_add_seg
			call set_start_art
			jmp @@args
			
			@@damage_art:
			mov ax, offset started_damage
			call set_start_art
			jmp @@args
			
			@@dead_art:
			mov ax, offset started_dead
			call set_start_art
			jmp @@args
			
			@@parse_letter:
			cmp bl, 'h'
			jz @@help
			cmp bl, 'H'
			jz @@help
			cmp bl, 'i'
			jz @@intersection
			cmp bl, 'I'
			jz @@intersection
			cmp bl, 'a'
			jz @@add_seg_art
			cmp bl, 'A'
			jz @@add_seg_art
			cmp bl, 'l'
			jz @@damage_art
			cmp bl, 'L'
			jz @@damage_art
			cmp bl, 'd'
			jz @@dead_art
			cmp bl, 'D'
			jz @@dead_art
			
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

intersection proc	;si - байт перед пробелом и параметром
	push ax
	push bx
	
	add si, 2
	mov bl, 10
	call parse_str_to_int
	jc @@err
	cmp al, 2
	jg @@err
	jmp @@next
	
@@err:
	mov al, 3
	call end_with_err
	
@@next:
	mov [mode_intersection_with_yourself], al
	
	pop bx
	pop ax
	ret
intersection endp

set_start_art proc		;ax - место, куда записывать
	push ax
	push bx
	push di
	
	mov di, ax
	
	add si, 2
	mov bl, 10
	call parse_str_to_int
	jnc @@next
	
	mov al, 3
	call end_with_err
	
@@next:
	mov [di], al
	
	pop di
	pop bx
	pop ax
	ret
set_start_art endp

;АРГУМЕНТЫ КОМАНДНОЙ СТРОКИ===============================
;Последний код ошибки: 04h
;01 - 

mode_intersection_with_yourself db 0
started_add_seg db 1
started_damage db 0
started_dead db 0

;ГЛОМАБЛЬНЫЕ ПАРАМЕТРЫ====================================
background_color equ 0
graph_buffer equ 0B800h
work_mode equ 04h
work_page equ 0
draw_width equ 80
skip_ticks_to_handle_int08 equ 2
min_skip_upd equ 1
max_skip_upd equ 6
start_skip_upd equ 3 

cur_mode db 0
cur_page db 0
handle_tick db 0

on_pause db 0
skip_update db 0
current_game_state db 0

state_continue equ 0
state_lose equ 1
state_win equ 2
state_hard_leave equ 3

old_int_9 	dw 0, 0

;СООБЩЕНИЯ================================================

help_msg db "-h -H", 09h, 09h, "print help", 0Ah
		 db "-i -I", 09h, 09h, "type intersection with yourself (0-no, 1-dead)$"
error_msg db "Incorrect program state. Code:"
error_code db "  h"
endl db 0dh, 0ah, 24h

end _start

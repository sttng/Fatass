menu_ff42		equ menu_ram+$00	; $01 bytes
menu_num_sel		equ menu_ram+$01	; $01 bytes
menu_cur_sel		equ menu_ram+$02	; $01 bytes
menu_cur_y_pos		equ menu_ram+$03	; $01 bytes

menu_height_sel		equ menu_ram+$04	; $01 bytes

main_menu_sel		equ menu_ram+$05	; $01 bytes
help_menu_sel		equ menu_ram+$06	; $01 bytes

efx_menu_sel		equ menu_ram+$07	; $01 bytes
efx_x_menu_sel		equ menu_ram+$08	; $01 bytes

conf_menu_sel		equ menu_ram+$09	; $01 bytes
conf_x_menu_sel		equ menu_ram+$0a	; $01 bytes

load_from_rom_menu_sel	equ menu_ram+$0b	; $01 bytes
load_from_sram_menu_sel	equ menu_ram+$0c	; $01 bytes
save_from_sram_menu_sel	equ menu_ram+$0d	; $01 bytes
delete_from_sram_menu_sel	equ menu_ram+$0e	; $01 bytes

seq_efx_menu_sel	equ menu_ram+$0f	; $01 bytes
seq_efx_x_menu_sel	equ menu_ram+$10	; $01 bytes

main_menu_came_from	equ menu_ram+$11	; $01 bytes
menu_raster_pos		equ menu_ram+$12	; $02 bytes

;***************************************************************************
;---------------------------------------------------------------------------
; menu setup
; call with number of selections in A
; and address of map in DE
;---------------------------------------------------------------------------
;***************************************************************************

setup_menu:

	; calculate $ff42 range

	ld (menu_num_sel),a

	ld a,(current_rom_bank)
	push af

	ld hl,$0d43

	ld a,c
	ld (menu_height_sel),a
	ld a,b
	ld (menu_cur_sel),a
	and a
	jr z,_done_setup_pos

_setup_pos_loop:
	
	ld a,l
	cp c
	jr z,_move_down
	add a,$08
	ld l,a
	dec b
	jr nz,_setup_pos_loop

_move_down:
	ld a,h
	add a,$08
	ld h,a
	dec b
	jr nz,_move_down

_done_setup_pos:

	ld a,l
	ld (menu_cur_y_pos),a
	ld a,h
	ld (menu_ff42),a

	ld a,%11011011	; LCD Controller = On
			; WindowBank = $9c00 (Not used)
			; Window = Off
			; BG Chr = $8000
			; BG Bank= $9c00
			; OBJ    = 8x8
			; OBJ    = On
			; BG     = On
	ld ($ff40),a

	ld hl,$9d01
	ld bc,$1812
	ld a,mapbank00
	call scr_copy_to_vram

	call palette_fix_last_entry

	call clear_cursor
	call set_menu_bar

	ld a,(gbc)
	and a
	jr z,_dmg_setup

	ld a,$14
	ld (joy_repeat_time_max),a
	ld a,$04
	ld (joy_repeat_time_min),a

	ld hl,$9cc0
	ld de,menu_header_gbc_map
	ld bc,$0214
	ld a,mapbank00
	call scr_copy_to_vram

	pop af
	ld (current_rom_bank),a
	ld ($2666),a
	ret

_dmg_setup:

	ld a,$14/2
	ld (joy_repeat_time_max),a
	ld a,$04/2
	ld (joy_repeat_time_min),a

	ld hl,$9cc0
	ld de,menu_header_dmg_map
	ld bc,$0214
	ld a,mapbank00
	call scr_copy_to_vram_dmg

	pop af
	ld (current_rom_bank),a
	ld ($2666),a
	ret

;***************************************************************************
;---------------------------------------------------------------------------
; menu poop loop
;---------------------------------------------------------------------------
;***************************************************************************

menu_poop_loop:

;---------------------------------------------------------------------------

	call update_waveforms
	call joy_repeat_update

	ld a,(joy_held)
	bit joy_bit_a,a
	ret nz

;---------------------------------------------------------------------------

	ld a,(joy_held_repeat)
	bit joy_bit_up,a
	jr z,_not_up

	ld a,(menu_cur_sel)
	and a
	ret z
	dec a
	ld (menu_cur_sel),a

	ld a,(menu_cur_y_pos)
	cp $43
	jr z,_move_up
	sub $08
	ld (menu_cur_y_pos),a
	ret

_move_up:
	ld a,(menu_ff42)
	sub $08
	ld (menu_ff42),a
	ret

;---------------------------------------------------------------------------

_not_up:

	bit joy_bit_down,a
	ret z

	ld a,(menu_num_sel)
	ld b,a
	ld a,(menu_cur_sel)
	cp b
	ret z
	inc a
	ld (menu_cur_sel),a

	ld a,(menu_height_sel)
	ld c,a
	ld a,(menu_cur_y_pos)
	cp c
	jr z,_move_down
	add a,$08
	ld (menu_cur_y_pos),a
	ret

_move_down:
	ld a,(menu_ff42)
	add a,$08
	ld (menu_ff42),a

	ret
	
;---------------------------------------------------------------------------

;***************************************************************************

;***************************************************************************
;---------------------------------------------------------------------------
; menu vbl irq
;---------------------------------------------------------------------------
;***************************************************************************

menu_vbl_irq:

	call buffer_cursor_colours

	ld a,(menu_cur_y_pos)
	sub $10
	ld (menu_raster_pos),a

	ei
	call editor_calc_waves
	jp done_vbl

;***************************************************************************
;---------------------------------------------------------------------------
; menu mode lcdc irq
;---------------------------------------------------------------------------
;***************************************************************************

;---------------------------------------------------------------------------
; main lcdc irq controller

menu_lcdc_irq:

	ld a,($ff44)
	cp $8d
	jp nc,c_wait_6
	cp $8a
	jp nc,c_wait_5
	cp $35
	jr nc,_wait_3
	cp $33-1
	jr nc,_wait_2q
	cp $24
	jr nc,mc_wait_2
	cp $02
	jp c,c_wait_1
	jp logo_lcdc_irq

;---------------------------------------------------------------------------

_wait_2q:
	push bc

	ld a,(menu_raster_pos)
	ld c,a
	ld hl,cursor_colour_buffer
	ld b,8

	ld a,$38+$80
	ld ($ff68),a

_wait_2qa:
	ld a,($ff44)
	cp $33
	jr c,_wait_2qa

	ld a,(menu_ff42)
	ld ($ff42),a

	ld a,c
	cp $38
	jr c,_wait_3_go

	dec a
	ld ($ff45),a

	pop bc
	pop hl
	pop af
	reti

;---------------------------------------------------------------------------

_wait_3:

	push bc

	ld a,(menu_raster_pos)
_wait_3_q:
	ld c,a

	ld hl,cursor_colour_buffer
	ld b,8

_wait_3d:
	ld a,$38+$80
	ld ($ff68),a

_wait_3q:
	ld a,($ff44)
	cp c
	jr c,_wait_3q

_wait_3_go:

	ld a,(hli)
	ld ($ff69),a
	ld a,(hli)
	ld ($ff69),a

	inc c
	dec b

	jr nz,_wait_3d
	
	ld hl,colour_table+$04
	ld a,$38+$80
	ld ($ff68),a

_wait_3r:
	ld a,($ff44)
	cp c
	jr c,_wait_3r

	ld a,(hli)
	ld ($ff69),a
	ld a,(hl)
	ld ($ff69),a

	ld a,c

	pop bc

	cp $8b-1
	jr nc,c_wait_5

	ld a,$8b-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; fade out colours at bottom and also draw bottom of window

c_wait_5:		
	ld a,($ff44)
	cp $8b
	jr c,c_wait_5

	ld a,$b0
	ld ($ff42),a

	ld a,$90-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; switch from top window to main edit and fade in edit text

mc_wait_2:
	ld a,($ff44)
	cp $2f
	jr c,mc_wait_2

	ld a,2
	ld ($ff42),a

	ld a,$33-1
	ld ($ff45),a

	pop hl
	pop af
	reti


;***************************************************************************
;---------------------------------------------------------------------------
; menu mode lcdc dmg
;---------------------------------------------------------------------------
;***************************************************************************

;---------------------------------------------------------------------------
; main lcdc dmg controller

menu_lcdc_dmg:

	ld a,($ff44)
	cp $8d
	jp nc,d_wait_6
	cp $74
	jr nc,d_wait_5
	cp $32-1
	jr nc,_wait_2q
	cp $24
	jr nc,md_wait_2
	cp $02
	jp c,d_wait_1
	pop hl
	pop af
	reti	

;---------------------------------------------------------------------------

_wait_2q:
	ld a,($ff44)
	cp $32
	jr c,_wait_2q

	ld a,%11010011
	ld ($ff47),a

_wait_02r:
	ld a,($ff44)
	cp $33
	jr c,_wait_02r

	ld a,(menu_ff42)
	ld ($ff42),a

	ld a,$8b-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; switch from top window to main edit and fade in edit text

md_wait_2:
	ld a,($ff44)
	cp $30
	jr c,md_wait_2

	ld a,2
	ld ($ff42),a

	ld a,%11010011
	ld ($ff47),a

	ld a,$32-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;---------------------------------------------------------------------------
; fade out colours at bottom and also draw bottom of window

d_wait_5:		
	ld a,($ff44)
	cp $8b
	jr c,d_wait_5

	ld a,$b0
	ld ($ff42),a

	ld a,$90-1
	ld ($ff45),a

	pop hl
	pop af
	reti

;***************************************************************************

	end
